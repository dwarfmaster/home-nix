{
  users.users.luc = {
    uid = 1000;
    password = "";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" "luc" ];
  };

  home-manager.users.luc = {
    imports = [
      ../profiles/packages
      ../profiles/git
    ];

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";
  };
}
