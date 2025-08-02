{
  config,
  pkgs,
  lib,
  ...
}: let
  lsd = "${pkgs.lsd}/bin/lsd";
  bat = "${pkgs.bat}/bin/bat";
in {
  # Misc
  home = {
    sessionVariables = {
      EDITOR = "nvim";
      DIRSTACKSIZE = 16;
      NIX_SSHOPTS = "--ask-sudo-password";
      MANROFFOPT = "-c"; # Fix problems with using bat as pager
      MANPAGER = "sh -c 'col -bx | ${bat} -l man -p'";
    };
    packages = builtins.attrValues {
      inherit
        (pkgs)
        lsd
        du-dust
        dua
        duf
        choose
        sd
        bottom
        hyperfine
        gping
        dogdns
        ;
      batgrep = pkgs.bat-extras.batgrep;
    };
  };
  programs.bat.enable = true;
  stylix.targets.bat.enable = true;

  # Bash
  programs.bash = {
    enable = true;
    initExtra = ''
      # LS_COLORS setting
      eval $(dircolors ${./dircolors})
    '';

    historyControl = ["erasedups"];
    historyFile = "${config.xdg.cacheHome}/bash/history";
    historyIgnore = ["ls" "ll" "exit"];

    shellAliases = {
      dh = "dirs -v";
      ls = "ls --color=auto";
      ll = "ls -lrth";
      lla = "ll -A";
      lld = "ll /dev/sd*";
      rm = "rm --preserve-root -i";
      cat = "bat";
      df = "duf";
      ping = "gping";
    };
  };

  # Zsh
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    dotDir = "${lib.removePrefix config.home.homeDirectory config.xdg.configHome}/zsh";
    defaultKeymap = "viins";

    history = rec {
      expireDuplicatesFirst = true;
      ignoreSpace = true;
      extended = true;
      save = 16192;
      size = save;
      share = true;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    # Enable Powerlevel10k instant prompt. Should stay close to the top of
    # .zshrc. Initialization code that may require console input (password
    # prompts, [y/n] confirmations, etc.) must go above this block; everything
    # else may go below.
    initContent = lib.mkMerge [
      (lib.mkBefore ''
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
        if [[ -r "${config.xdg.cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "${config.xdg.cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi
      '')
      (''
        # Using directory stacks as directory history : zsh.sourceforge.net/Intro/intro_6.html
        setopt autopushd pushdminus pushdsilent pushdtohome

        # Change up/down keys behaviour : match commands in history by first letters
        bindkey 'OA' history-beginning-search-backward
        bindkey 'OB' history-beginning-search-forward

        # LS_COLORS setting
        eval $(dircolors ${./dircolors})

        # P10K config
        if zmodload zsh/terminfo && (( terminfo[colors] >= 256 )); then
          # capable terminal
          source ${./p10k.zsh}
          alias ls=${lsd}
        else
          # might be TTY or some other not very capable terminal
          source ${./p10k-tty.zsh}
          alias ls='ls --color=auto'
        fi
      '')
    ];
    shellAliases = {
      dh = "dirs -v";
      ll = "ls -lrth";
      lla = "ll -A";
      lld = "ll /dev/sd*";
      rm = "rm --preserve-root -i";
      cat = "bat";
      df = "duf";
      ping = "gping";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git" # Add lots of git aliases
        "sudo"
        "dirpersist" # Saves dir stack across zsh reboots
        "extract" # Define a function extract that can extract any archive
        "pass" # Autocompletion for pass
        "jump" # Allow to mark directories and jump to them
        "gitignore" # Add a gi command to download templates from gitignore.io
        "cabal" # Autocompletion for cabal
        "emoji-clock" # Add a function emoji-clock that display a fancy clock
      ];
    };
  };
}
