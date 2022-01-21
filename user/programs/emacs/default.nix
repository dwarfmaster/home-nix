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
  programs.doom-emacs = {
    config = {
      enable = true;
      initModules = import ./init.nix;
      modules.config.main = {
        packages.source = ./packages.el;
        config.source = ./config.el;
      };
    };
  };
  xdg.configFile."doom".source = config.programs.doom-emacs.config.dir;
  
  home.packages = with pkgs; [
    gvfs # Necessary for TRAMP support for webdav
    ripgrep
    sqlite
    wordnet
    fd
    # Spell checkers
    aspell
    (hunspellWithDicts (with hunspellDicts; [ fr-any en_US en_CA en_AU ]))
  ];


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
    Icon=emacs
  '';
  # Create desktop entry for emacsclient
  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=Emacs Client
    Exec=${client} -c
    Type=Application
    Terminal=false
    Categories=System;Editor;
    Icon=emacs
  '';

  home.sessionVariables = {
    GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
  };
}
