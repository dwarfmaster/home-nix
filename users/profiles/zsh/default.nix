{ pkgs, ... }:

{
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

    initExtra        = builtins.readFile ./zshrc;
    localVariables   = import ./shell-variables.nix;
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
      custom = "\$XDG_CONFIG_HOME/zsh-custom";
      plugins = [
        "git"                   # Add lots of git aliases
        "sudo"
        "cp"                    # Add a cpv command that works likes cp but with progress bar
        "colored-man-pages"
        "dirpersist"            # Saves dir stack across zsh reboots
        "extract"               # Define a function extract that can extract any archive
        "pass"                  # Autocompletion for pass
        # "per-directory-history" # Save history per directory, allow switching with ^G
        "fasd"                  # Add a f command that select files based on "frecency", and a j command to cd
        "jump"                  # Allow to mark directories and jump to them
        "gitignore"             # Add a gi command to download templates from gitignore.io
        "cabal"                 # Autocompletion for cabal
        "emoji-clock"           # Add a function emoji-clock that display a fancy clock
      ];
      theme = "spaceship";
    };
  };

  xdg.configFile."zsh-custom"    .source = ./custom;
  xdg.configFile."zsh/dircolors" .source = ./dircolors;

  home = {
    sessionVariables = {
      EDITOR    = "vim";
      NIX_PATH  = "nixpkgs=$HOME/.config/nixpkgs/exposed/nixpkgs:nixpkgs-overlays=$HOME/.config/nixpkgs/exposed/overlays";
      DIRSTACKSIZE = 16;
    };
    sessionPath = [ "$HOME/.emacs.d/bin" ];
  };
}

