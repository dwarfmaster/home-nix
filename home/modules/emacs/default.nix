general@{ lib, recdata, ... }:

let

  unstable = general.pkgs.nixpkgs.nixos-unstable;

  pkgs = general.pkgs.main;

  epkgs = epkgs: with epkgs; [
      evil
      evil-surround
      smooth-scrolling
      lsp-mode
      lsp-ui
      epkgs.general
      nix-mode
      helm
      hledger-mode
      company
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
      elfeed
      elfeed-org
      elfeed-goodies
      tramp
      orgit
      magit-annex
      direnv
      use-package
      flycheck
      attrap

      # Haskell
      haskell-mode
      dante
      structured-haskell-mode

      # Color scheme
      base16-theme
      cyberpunk-theme
  ];

in {
  programs.emacs = {
    enable = true;
    # Workaround because of https://github.com/rycee/home-manager/issues/1300
    package = unstable.emacsWithPackages epkgs;
  };

  home.file.".emacs".text = ''
    (add-to-list 'load-path "/home/luc/.config/emacs/")
    (require 'main)
  '';

  xdg.configFile."emacs/main.el".source = ./main.el;
  xdg.configFile."emacs/ob-hledger.el".source = ./ob-hledger.el;
  xdg.configFile."emacs/nixpaths.el".text = ''
    (setq nix/figlet "${pkgs.figlet}/bin/figlet")
    (setq nix/curl "${pkgs.curl}/bin/curl")
    (setq nix/coqtop "${pkgs.coq}/bin/coqtop")
    (setq nix/sqlite3-bin-dir "${pkgs.sqlite}/bin")
    (setq nix/zathura "${pkgs.zathura}/bin/zathura")
    (setq nix/firefox "${pkgs.firefox}/bin/zathura")
    (setq nix/hledger "${pkgs.hledger}/bin/hledger")
    (setq nix/git "${pkgs.git}/bin/git")
    (setq nix/xdg-open "${pkgs.xdg_utils}/bin/xdg-open")

    (provide 'nixpaths)
  '';

  # Set emacsclient as the org-protocol:// url handler
  # TODO use finalPackage
  xdg.dataFile."applications/org-protocol.desktop".text = ''
    [Desktop Entry]
    Name=org-protocol
    Exec=emacsclient %u
    Type=Application
    Terminal=false
    Categories=System;
    MimeType=x-scheme-handler/org-protocol;
  '';

  packages = with pkgs; [
    gvfs # Necessary for TRAMP support for webdav
    haskellPackages.structured-haskell-mode
  ];

  shellVariables = {
    GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
  };

  haskellPackages = [ (hpkgs: with hpkgs; [ hindent ]) ];
}

