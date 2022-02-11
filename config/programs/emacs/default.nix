{ pkgs, config, ... }:

let

  cfg = config.programs.doom-emacs;
  doom = cfg.package;
  emacs = "${doom}/bin/emacs";
  client = "${doom}/bin/emacsclient";
  epkgs = pkgs.emacsPackages;

in {
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = cfg.config.dir;
    config = {
      enable = true;
      initModules = import ./init.nix;
      modules.config.main = {
        packages.source = ./packages.el;
        config.source = ./config.el;
      };
    };

    # Fix some mismatch between packages names
    # See https://github.com/vlaci/nix-doom-emacs/issues/394#issuecomment-985368661
    emacsPackagesOverlay = self: super: {
      gitignore-mode = epkgs.git-modes;
      gitconfig-mode = epkgs.git-modes;
    };
  };

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
      Type = "notify";
      ExecStart = "${pkgs.runtimeShell} -l -c '${emacs} --fg-daemon'";
      SuccessExitStatus=15;
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
