{ config, pkgs, lib, ... }:

let
  inherit (pkgs) unstable;
  lsd = "${pkgs.lsd}/bin/lsd";
in {
  # Misc
  home = {
    sessionVariables = {
      EDITOR    = "vim";
      DIRSTACKSIZE = 16;
    };
    packages = [ pkgs.lsd ];
  };
  xdg.configFile."ls/dircolors".source = ./dircolors;

  # Bash
  programs.bash = {
    enable = true;
    initExtra = ''
      # LS_COLORS setting
      eval $(dircolors ${config.xdg.configHome}/ls/dircolors)
    '';

    historyControl = [ "erasedups" ];
    historyFile = "${config.xdg.cacheHome}/bash/history";
    historyIgnore = [ "ls" "ll" "exit" ];

    shellAliases     = {
      dh      = "dirs -v";
      ls      = "ls --color=auto";
      ll      = "ls -lrth";
      lla     = "ll -A";
      lld     = "ll /dev/sd*";
      rm      = "rm --preserve-root -i";
    };
  };

  # Zsh
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
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
    initExtraFirst = ''
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      if [[ -r "${config.xdg.cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "${config.xdg.cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
    '';
    # TODO make reupload its own script
    initExtra = ''
      # Using directory stacks as directory history : zsh.sourceforge.net/Intro/intro_6.html
      setopt autopushd pushdminus pushdsilent pushdtohome

      # Change up/down keys behaviour : match commands in history by first letters
      bindkey 'OA' history-beginning-search-backward
      bindkey 'OB' history-beginning-search-forward

      # reMarkable uploading
      function reupload() {
      	curl 'http://10.11.99.1/upload' -H 'Origin: http://10.11.99.1' -H 'Accept: */*' \
      		-H 'Referrer: http://10.11.99.1/' -H 'Connection: keep-alive'           \
      		-F "file=@$1;filename=$1;type=application/pdf"
      }

      # LS_COLORS setting
      eval $(dircolors ${config.xdg.configHome}/ls/dircolors)

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
    '';
    shellAliases     = {
      dh      = "dirs -v";
      ll      = "ls -lrth";
      lla     = "ll -A";
      lld     = "ll /dev/sd*";
      rm      = "rm --preserve-root -i";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"                   # Add lots of git aliases
        "sudo"
        "cp"                    # Add a cpv command that works likes cp but with progress bar
        "colored-man-pages"
        "dirpersist"            # Saves dir stack across zsh reboots
        "extract"               # Define a function extract that can extract any archive
        "pass"                  # Autocompletion for pass
        "fasd"                  # Add a f command that select files based on "frecency", and a j command to cd
        "jump"                  # Allow to mark directories and jump to them
        "gitignore"             # Add a gi command to download templates from gitignore.io
        "cabal"                 # Autocompletion for cabal
        "emoji-clock"           # Add a function emoji-clock that display a fancy clock
      ];
    };
  };

  # NuShell
  programs.nushell = {
    enable = true;
    package = unstable.nushell;
    # TODO configure
    settings = { };
  };

  # Theme
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    # TODO nushell integration
    settings = import ./starship.nix;
  };
}
