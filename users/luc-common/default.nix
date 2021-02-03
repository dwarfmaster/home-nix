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
      # Desktop
      ../profiles/xdg
      ../profiles/fzf

      # Shell
      ../profiles/git
      ../profiles/direnv
      ../profiles/vim
      ../profiles/zsh

      # Security
      ../profiles/encryption
    ];

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";
  };
}
