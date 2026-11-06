{
  description = "org-electric";

  inputs = {
    emacs-package-flake = {
      url = "github:sucso/emacs-package-flake";
    };
  };

  outputs = { self, emacs-package-flake }:
    emacs-package-flake.lib.mkOutputs {
      name = "org-electric";
      src = ./.;
      optionalDependencies = [ "auctex" "cdlatex" ];
    };
}
