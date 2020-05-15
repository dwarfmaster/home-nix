general:

let
  pkgs = general.pkgs.main;
  mpdPort = 6600;
in {
  # Music player daemon
  services.mpd = {
    enable         = true;
    musicDirectory = "/home/luc/div/music";
    network = {
      listenAddress = "127.0.0.1";
      port          = mpdPort;
    };
  };

  # Support for MPRIS2, a standard D-BUS interface for music players
  services.mpdris2 = {
    enable         = false; # Seems not to be working
    multimediaKeys = true; # Enable support for multimedia keys
    notifications  = true; # Enable song change notifications
    mpd = {
      host = "127.0.0.1";
      port = mpdPort;
    };
  };

  packages = with pkgs; [
    mpc_cli                      # MPC command line client
    vimpc                        # Vi-like curses client
    python37Packages.mps-youtube # NCurses youtube music player, can be made to use mpd
  ];

  # TODO automate the configuration of mpsyt
  # For now we must use the following commands from inside it :
  #   set player mpc
  #   set playerargs add

  xdg.configFile."vimpc/vimpcrc".source = ./vimpcrc;
}

