general@{ lib, recdata, ... }:

let

  pkgs = general.pkgs.main;

  doom-emacs = general.pkgs.doom-emacs {
    doomPrivateDir = ./doom;
  };

in {
  packages = with pkgs; [
    doom-emacs
    gvfs # Necessary for TRAMP support for webdav
    haskellPackages.structured-haskell-mode
    ccls
    texlab
    ripgrep
    sqlite
    wordnet
  ];


  home.file.".emacs.d/init.el".text = ''
  '';

  # xdg.configFile."emacs/main.el".source = ./main.el;
  # xdg.configFile."emacs/ob-hledger.el".source = ./ob-hledger.el;
  # xdg.configFile."emacs/nixpaths.el".text = ''
  #   (setq nix/figlet "${pkgs.figlet}/bin/figlet")
  #   (setq nix/curl "${pkgs.curl}/bin/curl")
  #   (setq nix/coqtop "${pkgs.coq}/bin/coqtop")
  #   (setq nix/sqlite3-bin-dir "${pkgs.sqlite}/bin")
  #   (setq nix/zathura "${pkgs.zathura}/bin/zathura")
  #   (setq nix/firefox "${pkgs.firefox}/bin/zathura")
  #   (setq nix/hledger "${pkgs.hledger}/bin/hledger")
  #   (setq nix/git "${pkgs.git}/bin/git")
  #   (setq nix/xdg-open "${pkgs.xdg_utils}/bin/xdg-open")

  #   (provide 'nixpaths)
  # '';

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

  shellVariables = {
    GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
  };

  haskellPackages = [ (hpkgs: with hpkgs; [ hindent ]) ];
}

