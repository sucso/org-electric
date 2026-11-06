;;; org-electric-test.el --- ERT tests for org-electric.el -*- lexical-binding: t; -*-

;; * prelims

(require 'ert)
(require 'org-electric)

(defvar org-electric-test--buffer nil
  "Persistent buffer used for running `org-electric' test cases.")

;; Reset test buffer on file re-evaluation.
(when (buffer-live-p org-electric-test--buffer)
  (kill-buffer org-electric-test--buffer)
  (setq org-electric-test--buffer nil))

(defun org-electric-test--get-buffer ()
  "Return or initialize a persistent buffer for `org-electric' tests."
  (unless (buffer-live-p org-electric-test--buffer)
    (setq org-electric-test--buffer (get-buffer-create " *org-electric-test*"))
    (with-current-buffer org-electric-test--buffer
      (org-mode)
      (org-electric-mode 1)))
  org-electric-test--buffer)

(defmacro org-electric-test-with-buffer (initial &rest body)
  "Execute BODY in a shared `org-mode' buffer containing INITIAL text.

The function assumes INITIAL contains either zero, one, or two pipe
characters (\"|\"). If the INITIAL text contains at least a pipe
character, then point is moved to the first pipe (which is then deleted)
before executing BODY. If the INITIAL text contains two pipe characters,
then the text in-between them will be marked and point will be moved at
the position of the second pipe before executing BODY."
  (declare (indent 1))
  ;; Reuse buffer to avoid instantiating new Org buffers for each test:
  ;; initializing `org-mode' is expensive!.
  `(with-current-buffer (org-electric-test--get-buffer)
     (let ((inhibit-read-only t) last-command this-command)
       (widen)
       (erase-buffer)
       (deactivate-mark)
       (insert ,initial)
       (goto-char (point-min))
       (when (search-forward "|" nil t)
         (delete-char -1)
         (let ((start (point)))
           (when (search-forward "|" nil t)
             (delete-char -1)
             (set-mark start)
             (activate-mark))))
       ,@body)))

(defun org-electric-test--type-char (char &optional arg)
  "Simulate typing CHAR into `org-electric-self-insert' with prefix ARG."
  (let ((last-command-event char)
        (last-input-event char))
    (if (eq char ?\s)
        (progn
          (setq this-command #'org-electric-self-insert-space)
          (org-electric-self-insert-space))
      (setq this-command #'org-electric-self-insert)
      (org-electric-self-insert arg))
    (setq last-command this-command)))

(defun org-electric-test--should-equal (expected)
  "Assert buffer content and point match EXPECTED pattern.

The function assumes that the EXPECTED text contains exactly one pipe
character (\"|\") that marks the expected position of point."
  (let ((expected-point (string-search "|" expected))
        (clean-expected (string-replace "|" "" expected)))
    (should (equal
             (buffer-substring-no-properties (point-min) (point-max))
             clean-expected))
    (when expected-point
      (should (= (point) (1+ expected-point))))))

;; * pairs insertion

(ert-deftest org-electric-test-basic-pairing-bol ()
  "Test pairing at beginning of a line."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "|"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "%c|%c" char char)))))

(ert-deftest org-electric-test-basic-pairing-after-whitespace ()
  "Test pairing after whitespace on a non-blank line."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "Hello |"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "Hello %c|%c" char char)))))

(ert-deftest org-electric-test-basic-pairing-after-allowed-prefixes ()
  "Test basic pairing after allowed prefix delimiters.

The allowed prefix delimiters are described in the section on \"Text
Markup\" in the Org syntax reference document. The document is available
at the following remote URL: `https://orgmode.org/worg/org-syntax.html'.'"
  (dolist (prefix '("(" "\"" "'" "{" "-"))
    (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
      (org-electric-test-with-buffer (format "Text %s|" prefix)
        (org-electric-test--type-char char)
        (org-electric-test--should-equal (format "Text %s%c|%c" prefix char char))))))

(ert-deftest org-electric-test-pre-character-inhibition-word ()
  "Test that pairing is inhibited when preceded by word characters."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "word|"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "word%c|" char)))))

(ert-deftest org-electric-test-pre-character-inhibition-number ()
  "Test that pairing is inhibited when preceded by numbers."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "123|"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "123%c|" char)))))

(ert-deftest org-electric-test-pre-character-inhibition-punctuation ()
  "Test that pairing is inhibited when preceded by undocumented characters.

The \"undocumented\" characters are undocumented in the section on
\"Text Markup\" in the Org syntax reference document. The document is
available at the following remote URL:
`https://orgmode.org/worg/org-syntax.html'.'"
  (dolist (invalid-prefix '("." "," ":" "!" "]" "}"))
    (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
      (org-electric-test-with-buffer (format "text%s|" invalid-prefix)
        (org-electric-test--type-char char)
        (org-electric-test--should-equal (format "text%s%c|" invalid-prefix char))))))

(ert-deftest org-electric-test-inhibit-when-already-markup-leaf ()
  "Test that pair insertion is inhibited when already inside a markup element of the same type and the relevant markup element is a leaf of the `org-element' abstract syntax tree."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%cfoo | bar%c" char char)
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "%cfoo %c| bar%c" char char char)))))

(ert-deftest org-electric-test-inhibit-when-already-markup-inner ()
  "Test that pair insertion is inhibited when already inside a markup element of the same type and the relevant markup element is NOT a leaf of the `org-element' abstract syntax tree."
  (dolist (char '(?* ?/))
    (org-electric-test-with-buffer "/*foo | bar*/"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "/*foo %c| bar*/"  char)))))

;; * overtyping

(ert-deftest org-electric-test-overtyping-at-eol ()
  "Test stepping past the closing delimiter at the end of a line."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%cfoo|%c" char char)
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "%cfoo%c|" char char)))))

(ert-deftest org-electric-test-overtyping-before-space ()
  "Test stepping past the closing delimiter when followed by whitespace."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%cfoo|%c rest" char char)
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "%cfoo%c| rest" char char)))))

(ert-deftest org-electric-test-overtyping-before-punctuation ()
  "Test stepping past the closing delimiter when followed by valid POST punctuation.

The \"valid POST punctuation\" is described in the section on \"Text
Markup\" in the Org syntax reference document. The document is available
at the following remote URL:
`https://orgmode.org/worg/org-syntax.html'.'"
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (post '("." "," "-" ")" "[" "}"))
      (org-electric-test-with-buffer (format "%cfoo|%c%s" char char post)
        (org-electric-test--type-char char)
        (org-electric-test--should-equal (format "%cfoo%c|%s" char char post))))))

(ert-deftest org-electric-test-overtyping-inhibit-after-space ()
  "Test that stepping past the closing delimiter is inhibited if point is
preceded by whitespace."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%cfoo |%c" char char)
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "%cfoo %c|%c%c" char char char char)))))

(ert-deftest org-electric-test-overtyping-inhibit-after-alnum ()
  "Test that stepping past the closing delimiter is inhibited when point is
followed by alphanumeric chars."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%cfoo|%cbar" char char)
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "%cfoo%c|%cbar" char char char)))))

;; * allowed/inhibited contexts

(ert-deftest org-electric-test-inhibited-code-element ()
  "Inhibit pairing inside Org code markup."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "~code | text~"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "~code %c| text~" char)))))

(ert-deftest org-electric-test-inhibited-verbatim-element ()
  "Inhibit pairing inside Org verbatim markup."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "=verbatim | text="
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "=verbatim %c| text=" char)))))

(ert-deftest org-electric-test-inhibited-latex-fragment ()
  "Inhibit pairing inside inline LaTeX fragments."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "Formula \\(\\alpha | \\beta\\)"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "Formula \\(\\alpha %c| \\beta\\)" char)))))

(ert-deftest org-electric-test-inhibited-latex-fragment-dollar ()
  "Inhibit pairing inside (dollar-delimited) inline LaTeX fragments."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "Formula $\\alpha | \\beta$"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "Formula $\\alpha %c| \\beta$" char)))))

(ert-deftest org-electric-test-inhibited-latex-environment ()
  "Inhibit pairing inside LaTeX environments."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "\\begin{equation}\n  E = | mc^2\n\\end{equation}"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "\\begin{equation}\n  E = %c| mc^2\n\\end{equation}" char)))))

(ert-deftest org-electric-test-inhibited-comment-line ()
  "Inhibit pairing inside comment lines."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "# This is a comment | line"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "# This is a comment %c| line" char)))))

(ert-deftest org-electric-test-inhibited-comment-block ()
  "Inhibit pairing inside #+BEGIN_COMMENT blocks."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+BEGIN_COMMENT\ncomment | body\n#+END_COMMENT"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+BEGIN_COMMENT\ncomment %c| body\n#+END_COMMENT" char)))))

(ert-deftest org-electric-test-inhibited-example-block ()
  "Inhibit pairing inside #+BEGIN_EXAMPLE blocks."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+BEGIN_EXAMPLE\nexample | text\n#+END_EXAMPLE"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+BEGIN_EXAMPLE\nexample %c| text\n#+END_EXAMPLE" char)))))

(ert-deftest org-electric-test-inhibited-export-block ()
  "Inhibit pairing inside #+BEGIN_EXPORT blocks."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+BEGIN_EXPORT html\n<p>|text</p>\n#+END_EXPORT"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+BEGIN_EXPORT html\n<p>%c|text</p>\n#+END_EXPORT" char)))))

(ert-deftest org-electric-test-inhibited-fixed-width ()
  "Inhibit pairing in fixed-width lines starting with colon."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer ": formatted | line"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format ": formatted %c| line" char)))))

(ert-deftest org-electric-test-inhibited-keyword ()
  "Inhibit pairing inside keyword lines."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+TITLE: My | Document"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+TITLE: My %c| Document" char)))))

(ert-deftest org-electric-test-inhibited-node-property ()
  "Inhibit pairing inside property drawers."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer ":PROPERTIES:\n:CUSTOM_ID: my-|id\n:END:"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format ":PROPERTIES:\n:CUSTOM_ID: my-%c|id\n:END:" char)))))

(ert-deftest org-electric-test-inhibited-planning ()
  "Inhibit pairing on planning lines."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "* Heading\nSCHEDULED: <2026-09-03 |Thu>"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "* Heading\nSCHEDULED: <2026-09-03 %c|Thu>" char)))))

(ert-deftest org-electric-test-inhibited-non-org-src-block ()
  "Inhibit pairing inside non-Org src blocks (e.g. emacs-lisp)."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+BEGIN_SRC emacs-lisp\n(defun |foo ())\n#+END_SRC"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+BEGIN_SRC emacs-lisp\n(defun %c|foo ())\n#+END_SRC" char)))))

(ert-deftest org-electric-test-org-src-block-pairs-inside ()
  "Test that pairing works inside an org src-block."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "#+BEGIN_SRC org\n|\n#+END_SRC"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "#+BEGIN_SRC org\n%c|%c\n#+END_SRC" char char)))))

(ert-deftest org-electric-test-inhibited-link-target ()
  "Inhibit pairing inside Org link targets."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "[[http://example.com/|]]"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal (format "[[http://example.com/%c|]]" char)))))

(ert-deftest org-electric-test-inhibited-link-desc ()
  "Inhibit pairing inside Org link descriptions."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer "[[http://example.com/][|]]"
      (org-electric-test--type-char char)
      (org-electric-test--should-equal
       (format "[[http://example.com/][%c|]]" char)))))

;; * wrap region

(ert-deftest org-electric-test-region-wrapping-basic ()
  "Test wrapping an active region with Org markup pairs."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (exchange-p '(nil t))
      (org-electric-test-with-buffer "hello |world| text"
        (when exchange-p (exchange-point-and-mark))
        (org-electric-test--type-char char)
        (org-electric-test--should-equal
         (format
          (if exchange-p
              "hello |%cworld%c text"
            "hello %cworld%c| text")
          char char))))))

(ert-deftest org-electric-test-region-wrapping-trims-whitespace ()
  "Test that region wrapping skips leading and trailing whitespace inside mark."
  ;; Watch out: the strings of this test contain both spaces and tabs!
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (exchange-p '(nil t))
      (org-electric-test-with-buffer "hello |  world 	| text"
        (when exchange-p (exchange-point-and-mark))
        (org-electric-test--type-char char)
        (org-electric-test--should-equal
         (format
          (if exchange-p
              "hello   |%cworld%c 	 text"
            "hello   %cworld%c| 	 text")
          char char))))))

(ert-deftest org-electric-test-region-wrapping-whitespace ()
  "Test that wrapping a region containing only whitespace clobbers whitespace with `delete-selection-mode'."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (exchange-p '(nil t))
      (org-electric-test-with-buffer "hello |   | text"
        (when exchange-p (exchange-point-and-mark))
        (require 'delsel) ; delsel is built-in
        (delete-selection-mode)
        (org-electric-test--type-char char)
        (org-electric-test--should-equal
         (format (if exchange-p "hello %c| text" "hello %c| text") char))))))

(ert-deftest org-electric-test-region-wrapping-link ()
  "Test wrapping a region containing only an Org link."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (exchange-p '(nil t))
      (org-electric-test-with-buffer "|[[https://example.com][description]]|"
        (when exchange-p (exchange-point-and-mark))
        (org-electric-test--type-char char)
        (org-electric-test--should-equal
         (format
          (if exchange-p
              "|%c[[https://example.com][description]]%c"
            "%c[[https://example.com][description]]%c|")
          char char))))))

(ert-deftest org-electric-test-region-wrapping-latex-env ()
  "Test wrapping a region containing an inline latex environments snugly."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (dolist (exchange-p '(nil t))
      (org-electric-test-with-buffer "foo says |\(hello\)| world"
        (when exchange-p (exchange-point-and-mark))
        (org-electric-test--type-char char)
        (org-electric-test--should-equal
         (format
          (if exchange-p
              "foo says |%c\(hello\)%c world"
            "foo says %c\(hello\)%c| world")
          char char))))))

;; * delete-pair

(ert-deftest org-electric-test-delete-pair-simple ()
  "Test deleting a pair surrounding a single word."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "|%cfoobar%c" char char)
      (org-electric-delete-pair)
      (org-electric-test--should-equal "|foobar"))))

(ert-deftest org-electric-test-delete-pair-leading-whitespace ()
  "Test that deleting a markup pair leaves leading whitespace intact."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format " | %cfoobar%c" char char)
      (org-electric-delete-pair)
      (org-electric-test--should-equal " | foobar"))))

(ert-deftest org-electric-test-delete-pair-trailing-whitespace ()
  "Test that deleting a markup pair leaves trailing whitespace intact."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "|%cfoobar%c " char char)
      (org-electric-delete-pair)
      (org-electric-test--should-equal "|foobar "))))

(ert-deftest org-electric-test-delete-pair-nested-outer ()
  "Test that deleting an outer markup pair leaves the inner pair intact."
  (org-electric-test-with-buffer "|/*bold italic*/"
    (org-electric-delete-pair)
    (org-electric-test--should-equal "*bold italic*")))

(ert-deftest org-electric-test-delete-pair-nested-inner ()
  "Test that deleting an inner markup pair leaves the inner pair intact."
  (org-electric-test-with-buffer "/|*bold italic*/"
    (org-electric-delete-pair)
    (org-electric-test--should-equal "/|bold italic/")))

(ert-deftest org-electric-test-delete-pair-fallback ()
  "Test deleting a non-Org-markup pair."
  (org-electric-test-with-buffer "|(text)"
    (org-electric-delete-pair)
    (org-electric-test--should-equal "text")))

(ert-deftest org-electric-test-delete-pair-empty-inline ()
  "Test deleting a markup pair with no content."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "text |%c%c" char char)
      (org-electric-delete-pair)
      (org-electric-test--should-equal "text |"))))

(ert-deftest org-electric-test-delete-pair-empty-line-beg ()
  "Test deleting a markup pair with no content on an empty line."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    ;; Watch out! We are skipping asterisks: they are special, as they can be
    ;; used to declare lists or headings when inserted at the beginning of an
    ;; empty line. They are tested in `org-electric-test-space-at-stars'.
    (unless (eq char ?*)
      (org-electric-test-with-buffer (format "|%c%c" char char)
        (org-electric-delete-pair)
        (org-electric-test--should-equal "|")))))

;; * delete-char

(ert-deftest org-electric-test-delete-char-basic ()
  "Test that deleting the previous char deletes empty markup pairs."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "%c|%c" char char)
      (org-electric-delete-backward-char 1)
      (org-electric-test--should-equal "|"))))

(ert-deftest org-electric-test-delete-char-on-heading ()
  "Test that deleting the previous char deletes empty markup pairs on headings."
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "** %c|%c" char char)
      (org-electric-delete-backward-char 1)
      (org-electric-test--should-equal "** |"))))

;; * space

(ert-deftest org-electric-test-space-basic ()
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (unless (eq char ?*) ; covered by `org-electric-test-space-at-stars'
      (org-electric-test-with-buffer (format "%c|%c" char char)
        (org-electric-test--type-char ?\s)
        (org-electric-test--should-equal (format "%c |" char))))))

(ert-deftest org-electric-test-space-inline ()
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "text %c|%c" char char)
      (org-electric-test--type-char ?\s)
      (org-electric-test--should-equal (format "text %c |" char)))))

(ert-deftest org-electric-test-space-in-src ()
  (pcase-dolist (`(,char . ,_) org-electric-character-element-types)
    (org-electric-test-with-buffer (format "src_elisp{%c|%c}" char char)
      (org-electric-test--type-char ?\s)
      (org-electric-test--should-equal (format "src_elisp{%c |%c}" char char)))))

(ert-deftest org-electric-test-space-at-stars ()
  "Test that asterisks-then-space yields the expected number of asterisks.

The expected number of asterisks is, perhaps unsurprisingly, the number
of times the user hit the asterisk key."
  (dolist (n (number-sequence 1 5))
    (org-electric-test-with-buffer "|"
      (dotimes (_ n) (org-electric-test--type-char ?*))
      (org-electric-test--type-char ?\s)
      (org-electric-test--should-equal (concat (make-string n ?*) " |")))))

;; * cdlatex

(ert-deftest org-electric-test-cdlatex-underscore ()
  "Test that `org-cdlatex''s special underscore behaviour is preserved."
  ;; The test should pass regardless of the order of execution of the modes:
  ;; loading `org-cdlatex-mode' first and `org-electric-mode' second should
  ;; still pass, but it's recommended to load `org-electric-mode' last.
  (skip-unless (featurep 'cdlatex))
  (org-electric-test-with-buffer "\\(X|\\)"
    ;; Ensure `org-electric-mode' is loaded after `org-cdlatex-mode'.
    (org-electric-mode -1)
    (org-cdlatex-mode +1)
    (org-electric-mode +1)
    ;; Test.
    (org-electric-test--type-char ?_)
    (org-electric-test--type-char ?_)
    (org-electric-test--should-equal "\\(X_{\\mathrm{|}}\\)")
    ;; Cleanup: the buffer is persisted and used for all tests, and
    ;; `org-cdlatex-mode' might interfere with some tests.
    (org-cdlatex-mode -1)))

;; * end

(provide 'org-electric-test)

;; Local Variables:
;; outline-regexp: "\\(;; \\*+ \\)"
;; End:

;;; org-electric-test.el ends here
