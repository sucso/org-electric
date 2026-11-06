;;; org-electric.el --- Electric pairs for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Luca Prandini <luca.pr@mailbox.org>

;; Author: Luca Prandini <luca.pr@mailbox.org>
;; Maintainer: Luca Prandini <luca.pr@mailbox.org>
;; URL: https://github.com/luca-pr/org-electric
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: org, editing, convenience, electric
;; Description: A package for electric-mode behaviour in Org mode

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `org-electric' package provides `electric-pair-mode' behaviour for Org
;; emphasis markers when `org-electric-mode' is active.
;; Concretely, `org-electric' lets one perform the following operations.
;; - Insert pairs of emphasis markers by tapping their key once.
;; - Mark a region and quickly surround it with an emphasis marker.
;; - Delete contentless (adjacent) emphasis pairs by deleting the previous
;;   character when the cursor is in the middle of them.
;; - Delete pairs of emphasis markers by calling `org-electric-delete-pair'.
;; All operations occur only when the cursor's context is such that the Org
;; syntax specification (https://orgmode.org/worg/org-syntax.html) allows it;
;; for example, automatic pair insertion is inhibited inside code blocks.

;;; Code:

;; * Preliminaries

(require 'org)
(require 'org-keys) ; org-defkey
(require 'org-element)

(declare-function delete-selection-pre-hook "delsel")

(defconst org-electric-character-element-types
  '((?* . bold) (?/ . italic)
    (?= . verbatim) (?~ . code)
    (?+ . strike-through) (?_ . underline)))

(defconst org-electric-emphasis-element-types
  (mapcar #'cdr org-electric-character-element-types))

;; * Variables

(defvar org-electric-rebind-space t
  "Whether `org-electric-mode' rebinds `SPC' to `org-electric-self-insert-space'.")

(defvar org-electric-mode-map (make-sparse-keymap)
  "Keymap for `org-electric-mode'.")

(define-minor-mode org-electric-mode
  "Toggle automatic pairing of Org markup delimiters.

Org Electric mode is a buffer-local minor mode.  When enabled, typing an
Org markup delimiter (asterisk, forward slash, etc.) automatically
inserts the closing delimiter.  If the region is active, the delimiters
are inserted around the region instead."
  :lighter " OrgPair"
  :keymap org-electric-mode-map)

;; * Inhibition

(defun org-electric--strictly-in-element-p (element &optional epom)
  "Return non-nil if EPOM is within ELEMENT, excluding trailing whitespace."
  (let* ((begin (org-element-property :begin element))
         (end-without-post-blank (- (org-element-property :end element)
                                    (or (org-element-property :post-blank element) 0))))
    (and (> (or epom (point)) begin)
         (< (or epom (point)) end-without-post-blank))))

(defun org-electric--in-inhibited-context-p (&optional epom)
  "Return non-nil if EPOM is inside a context where Org markup pairing is invalid."
  (save-excursion
    (when epom (goto-char epom))
    (let* ((element (org-element-context))
           (type (org-element-type element)))
      ;; We are in an inhibited context if...
      (and
       ;; ...we are strictly inside the output of `org-element-context'...
       (when element (org-electric--strictly-in-element-p element))
       ;; ...and we are either...
       (or
        ;; ...in a non-Org (possibly inline) src-block...
        (and (memq type '(src-block inline-src-block))
             (if-let* ((lang (org-element-property :language element)))
                 (not (string-equal-ignore-case lang "org"))
               t))
        ;; ...or in one of the following elements.
        (memq type '(
                     code verbatim
                     latex-fragment latex-environment
                     comment comment-block
                     example-block export-block fixed-width
                     keyword node-property
                     clock planning)))))))

(defun org-electric--prematch-p ()
  "Return non-nil if the character preceding point is a valid Org markup prematch.

Prematch characters are defined by `org-emphasis-regexp-components'."
  (or (bolp)
      (let ((pre-chars (car org-emphasis-regexp-components))
            (char-before (char-to-string (char-before))))
        (string-match-p (concat "[" pre-chars "]") char-before))))

(defun org-electric--postmatch-p ()
  "Return non-nil if the character following point is a valid Org markup postmatch.

Postmatch characters are defined by `org-emphasis-regexp-components'."
  (or (eolp)
      (let ((post-chars (cadr org-emphasis-regexp-components))
            (char-after (char-to-string (char-after))))
        (string-match-p (concat "[" post-chars "]") char-after))))

(defun org-electric--at-stars-p ()
  "Return non-nil if point is within Org heading stars."
  (save-excursion
    (let ((p (point)))
      (beginning-of-line)
      (and (looking-at (rx (seq bol (one-or-more "*")) " "))
           (<= p (match-end 0))))))

(defun org-electric--insertion-p (char)
  "Return non-nil if inserting CHAR should trigger insertion of its matching pair."
  (and
   (org-electric--prematch-p)
   (not (org-electric--at-stars-p))
   (not (org-electric--in-inhibited-context-p))
   ;; Disallow nesting the same markup into itself (e.g. bold into bold).
   (not (org-element-lineage
         (org-element-context)
         `(,(alist-get char org-electric-character-element-types))
         t))))

(defun org-electric--skip-p (char)
  "Return non-nil if inserting CHAR should step over existing closing delimiter."
  (and
   (assq char org-electric-character-element-types)
   (eq char (char-after))
   (not (memq (char-syntax (char-before)) '(?\s)))
   (not (org-electric--at-stars-p))
   (save-excursion
     (forward-char +1)
     (and
      (not (org-electric--in-inhibited-context-p))
      (org-electric--postmatch-p)))))

;; * Insertion

(defmacro org-electric--self-insert-fallback ()
  "Run the fallback self-insert command without `org-electric-mode'."
  `(let* ((org-electric-mode nil)
          (cmd (or (key-binding (vector last-input-event))
                   #'org-self-insert-command)))
     (setq this-command cmd)
     (when delete-selection-mode
       (delete-selection-pre-hook))
     (call-interactively cmd)))

(defun org-electric-self-insert (arg)
  "Like `org-self-insert-command', with \"electric\" behaviour.

The numeric prefix argument ARG says how many times to repeat the
insertion.

`org-electric-mode' binds this command to the characters that appear in
`org-electric-character-element-types' as keys.

If the region is active, wrap the non-whitespace contents of the region
with the typed character; abort with an error if the region contains
only whitespace.

If point precedes the rightmost delimiter of an Org markup pair, skip
over it instead of inserting a new character.

If point is at a valid markup insertion context, insert a matching pair
of characters.

Otherwise, fallback to `org-self-insert-command' through
`org-electric--self-insert-fallback'."
  (interactive "P")
  (let ((char last-command-event)
        (count (prefix-numeric-value arg)))
    (cond
     ;; Attempt to wrap region.
     ((use-region-p)
      (if (org-electric--in-inhibited-context-p)
          (org-electric--self-insert-fallback)
        (atomic-change-group
          (let ((beg (region-beginning))
                (end (region-end)))
            (save-excursion
              (goto-char beg)
              (skip-chars-forward " \t\n")
              (setq beg (point))
              (goto-char end)
              (skip-chars-backward " \t\n")
              (setq end (point)))
            (if (> beg end)
                ;; Do not surround whitespace.
                (org-electric--self-insert-fallback)
              (let ((begp (<= (point) beg)))
                (goto-char end)
                (insert-char char)
                (goto-char beg)
                (insert-char char)
                (deactivate-mark)
                (if begp (goto-char beg) (goto-char (+ end 2)))))))))

     ;; Go past the second/rightmost delimiter of the pair. ("overtyping")
     ((org-electric--skip-p char)
      (forward-char 1))

     ;; Insert markup character pair.
     ((org-electric--insertion-p char)
      (atomic-change-group
        (org-electric--self-insert-fallback)
        (save-excursion (insert-char char count t))))

     ;; Fallback.
     (t (org-electric--self-insert-fallback)))))

(defun org-electric-self-insert-space ()
  "Like `org-self-insert-command', with \"electric\" behaviour."
  (interactive)
  (when (and (assq (char-before) org-electric-character-element-types)
             (eq (char-before) (char-after))
             (not (org-electric--in-inhibited-context-p)))
    (delete-char 1))
  (org-electric--self-insert-fallback))

;; * Deletion

(defun org-electric--delete-empty-pair ()
  "Attempt to delete empty emphasis pair surrounding point."
  (let* ((cb (char-before)) (ca (char-after)))
    (when (and (eq ca cb)
               (assq ca org-electric-character-element-types)
               (not (org-electric--at-stars-p)))
      (progn (forward-char +1)
             (funcall-interactively #'backward-delete-char 2)
             t))))

(defun org-electric--delete-pair (&optional check-beg-p)
  "Delete a pair of Org markup delimiters.

The characters defining pairs of Org markup delimiters recognized by
this function are those that appear in
`org-electric-character-element-types'.

With CHECK-BEG-P, deletion occurs only if point is at the beginning of
the markup object; otherwise, look for a surrounding Org markup object.

Return non-nil if a pair was deleted and nil otherwise."
  (let ((changed nil))
    (save-excursion
      (skip-chars-forward " \t")
      (if-let*
          ((context (org-element-context))
           (element (org-element-lineage context org-electric-emphasis-element-types t)))
          ;; Bold, italic, strike-through, and underline objects have contents,
          ;; but verbatim and code objects do not (according to `org-element',
          ;; at least). For this reason, the position of the delimiters cannot
          ;; be calculated with :contents-begin and :contents-end. Here we
          ;; leverage the :post-blank property instead.
          (let*
              ((beg (org-element-property :begin element))
               (end (org-element-property :end element))
               (post-blank (or (org-element-property :post-blank element) 0))
               (end (- end post-blank)))
            (when (or (not check-beg-p) (eq (point) beg))
              (delete-region (1- end) end)
              (delete-region beg (1+ beg))
              (when delete-pair-push-mark
                (push-mark (- end 2)))
              (setq changed t))))
      ;; No markup element found: check for an empty ("contentless") markup object.
      ;;
      ;; Three cases: "|**", "*|*", and everything else (no pair "near" point).
      ;; If CHECK-BEG-P, we want to delete the pair only in the first case.
      ;; To implement this, we leverage `org-electric--delete-empty-pair':
      ;; it performs pair deletion only in the second case.
      (unless check-beg-p
        ;; Attempt deletion; it will only succeed in the second case.
        (when (org-electric--delete-empty-pair) (setq changed t)))
      (unless changed
        ;; Skip this if we already deleted; otherwise, move the point forward.
        ;; If (and only if!) before moving point we were in the first case, we
        ;; are now in a second case scenario: calling
        ;; `org-electric--delete-empty-pair' does what needs to be done.
        (forward-char +1)
        (when (org-electric--delete-empty-pair) (setq changed t))))
    changed))

(defun org-electric-delete-pair (&optional arg)
  "Delete either Org markup delimiter pair or standard pair.

ARG is passed to `delete-pair', if the function is called.  If
`delete-pair' is not called by this function to delete a standard,
non-Org, pair, then setting ARG does nothing.

Deletion of Org markup delimiters occurs only if point is at the
beginning of the markup object.  If no Org markup object is detected,
fallback to `delete-pair'."
  (interactive "P")
  (unless (org-electric--delete-pair t)
    (funcall-interactively #'delete-pair arg)))

(defun org-electric-delete-backward-char (count)
  "Like `org-delete-backward-char', but with \"electric\" markup deletion.

COUNT is the number of previous characters to delete."
  (interactive "p")
  (while (> count 0)
    (unless (org-electric--delete-empty-pair) (org-delete-backward-char 1))
    (setq count (1- count))))

;; * Keybindings

(dolist (pair org-electric-character-element-types)
  (org-defkey org-electric-mode-map
              (kbd (single-key-description (car pair)))
              #'org-electric-self-insert))
(org-defkey org-electric-mode-map [remap delete-pair] #'org-electric-delete-pair)
(org-defkey org-electric-mode-map [remap delete-backward-char] #'org-electric-delete-backward-char)
(org-defkey org-electric-mode-map (kbd "SPC")
            '(menu-item "" org-electric-self-insert-space
                        :filter (lambda (cmd) (when org-electric-rebind-space cmd))))

;; * end

(provide 'org-electric)

;; Local Variables:
;; outline-regexp: "\\(;; \\*+ \\)"
;; sentence-end-double-space: t
;; End:

;;; org-electric.el ends here
