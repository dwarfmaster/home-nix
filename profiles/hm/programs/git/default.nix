{ pkgs, ... }:

let
  delta = "${pkgs.delta}/bin/delta";
in {
  home.packages = [ pkgs.delta ];

  programs.git = {
    enable = true;

    aliases = {
      co   = "checkout";
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
      core = {
        pager = "${delta}";
      };
      init = {
        defaultBranch = "main";
      };
      color = {
        diff        = "auto";
        status      = "auto";
        branch      = "auto";
        interactive = "auto";
      };
      push = {
        default = "matching";
      };
      merge = {
        conflictstyle = "diff3";
      };
      interactive = {
        diffFilter = "${delta} --color-only";
      };
      "add.interactive" = {
        useBuiltin = false;
      };
      diff = {
        colorMoved = true;
      };
      delta = {
        navigate = true;
        light = false;
      };
    };
    ignores = [ "*~" "*.swp" ".direnv" ];
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = [ "magit" ];
      ui = [ "vc-gutter" ];
      emacs = [ "vc" ];
    };
    modules.dwarfmaster.git-blamer = {
      config.source = ./blamer.el;
      packages.text = ''
        (package! blamer)
      '';
    };
  };
}


