{lib, ...}: {
  users.users.luc = {
    uid = 1000;
    hashedPassword = import ./password.nix;
    description = "default";
    isNormalUser = true;
    extraGroups = ["wheel" "luc" "networkmanager" "adbusers" "video" "dialout"];
    shell = "/run/current-system/sw/bin/zsh";
  };

  home-manager.users.luc = {
    profiles = {
      system = {
        xdg.enable = true;
        direnv.enable = true;
        encryption.enable = true;
      };
      interface.theme.enable = true;
      programs = {
        fzf.enable = true;
        git.enable = true;
        git-annex.enable = true;
        vim.enable = true;
        neovim.enable = true;
      };
    };

    xdg.enable = true;
    programs.git.userName = "DwarfMaster";
    programs.git.userEmail = "luc@dwarfmaster.net";

    manual = {
      html.enable = true;
      json.enable = true;
      manpages.enable = true;
    };
  };

  # Enable support for Ergodox EZ
  hardware.keyboard.zsa.enable = true;
}
