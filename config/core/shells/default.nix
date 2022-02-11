{ config, ... }:

let
  inherit (config.pkgsets) pkgs unstable;
in {
  # Misc
  home = {
    sessionVariables = {
      EDITOR    = "vim";
      DIRSTACKSIZE = 16;
    };
    # TODO move to emacs module
    sessionPath = [ "$HOME/.emacs.d/bin" ];
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
      ll      = "ls -lrthF";
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
    dotDir = ".config/zsh";
    defaultKeymap = "viins";

    history = rec {
      expireDuplicatesFirst = true;
      extended = true;
      save = 16192;
      size = save;
      share = true;
    };

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
    '';
    shellAliases     = {
      dh      = "dirs -v";
      ls      = "ls --color=auto";
      ll      = "ls -lrthF";
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
