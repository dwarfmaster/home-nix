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
