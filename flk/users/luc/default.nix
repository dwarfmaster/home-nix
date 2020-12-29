{
  users.users.luc = {
    uid = 1000;
    password = "";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" "luc" ];
  };

  home-manager.users.luc = {
    imports = [ ../profiles/packages ];
  };
}
