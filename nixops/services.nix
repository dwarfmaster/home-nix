{
  network.description = "DwarfMaster infrastructure";

  #  _____                      _ _ _ 
  # |_   _|   _ _ __   __ _  __| (_) |
  #   | || | | | '_ \ / _` |/ _` | | |
  #   | || |_| | | | | (_| | (_| | | |
  #   |_| \__,_|_| |_|\__, |\__,_|_|_|
  #                   |___/           
  # A laptop, used as a personal computer
  tungdil = {
    imports = [
      ./modules/general-misc.nix
      ./modules/personal-misc.nix
      ./modules/personal-packages.nix
      ./modules/openssh-server.nix
      ./modules/xserver.nix
      ./modules/users/luc.nix
    ];
    networking.hostName = "tungdil";
  };


  #  ____        _           _ _ _ 
  # | __ )  ___ (_)_ __   __| (_) |
  # |  _ \ / _ \| | '_ \ / _` | | |
  # | |_) | (_) | | | | | (_| | | |
  # |____/ \___/|_|_| |_|\__,_|_|_|
  #                                
  # A tower, used as a personal computer
  boindil = {
    imports = [
      modules/general-misc.nix
      modules/personal-misc.nix
      modules/personal-packages.nix
      modules/openssh-server.nix
      modules/xserver.nix
      modules/users/luc.nix
    ];
    networking.hostName = "boindil";
  };

}
