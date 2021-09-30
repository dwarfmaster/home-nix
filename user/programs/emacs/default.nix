{ pkgs, config, ... }:

# Doom needs to be installed manually

let

  doom = config.programs.emacs.finalPackage;

  emacs = "${doom}/bin/emacs";

  client = "${doom}/bin/emacsclient";

in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  
  home.packages = with pkgs; [
    gvfs # Necessary for TRAMP support for webdav
    haskellPackages.structured-haskell-mode
    ccls
    texlab
    ripgrep
    sqlite
    wordnet
    fd
    # Spell checkers
    aspell
    (hunspellWithDicts (with hunspellDicts; [ fr-any en_US en_CA en_AU ]))
  ];


  xdg.configFile."doom/init.el".source = ./init.el;
  xdg.configFile."doom/config.el".source = ./config.el;
  xdg.configFile."doom/packages.el".source = ./packages.el;

  # Systemd daemon
  systemd.user.services.doom-emacs-daemon = {
    Unit = {
      Description = "Doom Emacs Server Daemon";
      Documentation = [ "info:emacs" "man:emacs(1)" "https://gnu.org/software/emacs/" ];
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.dash}/bin/dash -c '${emacs} --daemon && ${client} -c --eval \"(delete-frame)\"'";
      ExecStop = "${client} --no-wait --eval \"(progn (setq kill-emacs-hook nil) (kill emacs))\"";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  # Set emacsclient as the org-protocol:// url handler
  xdg.dataFile."applications/org-protocol.desktop".text = ''
    [Desktop Entry]
    Name=org-protocol
    Exec=${client} %u
    Type=Application
    Terminal=false
    Categories=System;
    MimeType=x-scheme-handler/org-protocol;
  '';

  home.sessionVariables = {
    GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
  };
}

