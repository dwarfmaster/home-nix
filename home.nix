{ pkgs, ... }:

{
  home.packages = import ./packages.nix pkgs;

  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  };

  programs.firefox = {
    enable = true;
    enableIcedTea = true;
  };

  programs.zsh = import ./zsh.nix { };

  home.file.".xinitrc".source = ./xinitrc;
  home.file.".xmonad".source = ./xmonad;
  home.file.".xmonad".recursive = true;

  programs.git = {
    enable = true;
    userName = "DwarfMaster";
    userEmail = "luc@dwarfmaster.net";

    aliases = { co   = "checkout";
                st   = "status";
                br   = "branch";
                ci   = "commit";
                mg   = "merge";
                lg   = "shortlog";
                ls   = "ls-files";
                pl   = "pull --ff-only";
                rbpl = "pull --rebase";
                lol  = "log --graph --decorate --pretty='%C(yellow)%h%Creset -%C(bold cyan)%d%Creset %s %Cgreen(%cr)%Creset %C(bold blue)<%an>%Creset' --abbrev-commit";
                lola = "log --graph --decorate --pretty='%C(yellow)%h%Creset -%C(bold cyan)%d%Creset %s %Cgreen(%cr)%Creset %C(bold blue)<%an>%Creset' --abbrev-commit --all";
                mtdt = "annex metadata";
              };
    extraConfig = {
      color = {
        diff        = "auto";
        status      = "auto";
        branch      = "auto";
        interactive = "auto";
      };
      push = {
        default = "matching";
      };
    };
    ignores = [ "*~" "*.swp" ];
  };
}

