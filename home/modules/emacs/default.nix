general@{ lib, recdata, ... }:

let

  unstable = general.pkgs.nixpkgs.nixos-unstable;

  pkgs = general.pkgs.main;

  all-hies-gen = general.pkgs.hies;
  all-hies = all-hies-gen.selection { selector = p: { inherit (p) ghc865 ghc864 ghc863; }; };

  epkgs = epkgs: with epkgs; [
      evil
      evil-surround
      base16-theme
      smooth-scrolling
      lsp-mode
      lsp-ui
      lsp-haskell
      epkgs.general
      nix-mode
      helm
      hledger-mode
      company
      company-lsp
      idris-mode
      helm-idris
      proof-general
      company-coq
      projectile
      helm-projectile
      magit
      org
      helm-org
      hydra
      ts
      org-roam
      company-org-roam
      org-ref
      helm-bibtex
      org-roam-bibtex
  ];

in {
  programs.emacs = {
    enable = true;
    # Workaround because of https://github.com/rycee/home-manager/issues/1300
    package = unstable.emacsWithPackages epkgs;
  };

  home.file.".emacs".source = ./home.el;

  xdg.configFile."emacs/main.el".source = ./main.el;
  xdg.configFile."emacs/nixpaths.el".text = ''
    (setq nix/hie-wrapper "${all-hies}/bin/hie-wrapper")
    (setq nix/figlet "${pkgs.figlet}/bin/figlet")
    (setq nix/curl "${pkgs.curl}/bin/curl")
    (setq nix/coqtop "${pkgs.coq}/bin/coqtop")
    (setq nix/sqlite3-bin-dir "${pkgs.sqlite}/bin")
    (setq nix/zathura "${pkgs.zathura}/bin/zathura")
    (setq nix/firefox "${pkgs.firefox}/bin/zathura")
  '';
}

