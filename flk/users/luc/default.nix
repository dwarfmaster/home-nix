{
  users.users.luc = {
    uid = 1000;
    hashedPassword = import ./password.nix;
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" "luc" "networkmanager" "adbusers" "video" ];
    shell = "/run/current-system/sw/bin/zsh";
  };

  home-manager.users.luc = {
    imports = [
      ../profiles/packages
      ../profiles/git
      ../profiles/xdg
      ../profiles/x11
      ../profiles/fzf
      ../profiles/web
      ../profiles/direnv
      ../profiles/vim
      ../profiles/zsh
      ../profiles/emacs
    ];

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";
  };
}
