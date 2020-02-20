general@{ lib, self, recdata, ... }:

let
  pkgs = general.pkgs.main;
in {
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

    sessionVariables = lib.mayAccess [ "globalVariables" ] recdata;
    localVariables   = lib.mayAccess [ "shellVariables"  ] recdata;
    shellAliases     = lib.mayAccess [ "shellAliases"    ] recdata;
    initExtra        = builtins.readFile ./zshrc;

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

  globalVariables = {
    EDITOR    = "vim";
    NIX_PATH  = "nixpkgs=$HOME/.config/nixpkgs/pkgs/exposed.nix";
    DIRSTACKSIZE = 16;
  };

  shellVariables = {
    #  ____                       ____  _     _       
    # / ___| _ __   __ _  ___ ___/ ___|| |__ (_)_ __  
    # \___ \| '_ \ / _` |/ __/ _ \___ \| '_ \| | '_ \ 
    #  ___) | |_) | (_| | (_|  __/___) | | | | | |_) |
    # |____/| .__/ \__,_|\___\___|____/|_| |_|_| .__/ 
    #       |_|                                |_|    
    SPACESHIP_PROMPT_ORDER  = [ "dir" "git" "ruby" "rust" "haskell" "julia"
                                "line_sep" "vi_mode" "jobs" "exit_code" "char" ];
    SPACESHIP_RPROMPT_ORDER = [ "exec_time" "battery" "user" "host" "time" ];
    SPACESHIP_PROMPT_ADD_NEWLINE       = true;
    SPACESHIP_PROMPT_SEPARATE_LINE     = true;
    SPACESHIP_PROMPT_FIRST_PREFIX_SHOW = false;
    SPACESHIP_PROMPT_PREFIXES_SHOW     = true;
    SPACESHIP_PROMPT_SUFFIXES_SHOW     = true;
    SPACESHIP_CHAR_COLOR_SUCCESS       = "cyan";
    SPACESHIP_CHAR_COLOR_SECONDARY     = "magenta";

    SPACESHIP_TIME_SHOW        = true;
    SPACESHIP_TIME_COLOR       = "cyan";
    SPACESHIP_TIME_FORMAT      = "%* [%D]";
    SPACESHIP_DIR_SHOW         = true;
    SPACESHIP_DIR_TRUNC        = 4;
    SPACESHIP_DIR_TRUNC_PREFIX = ".../";
    SPACESHIP_DIR_TRUNC_REPO   = true;
    SPACESHIP_GIT_SHOW         = true;
    SPACESHIP_GIT_BRANCH_SHOW  = true;
    SPACESHIP_GIT_STATUS_SHOW  = true;
    SPACESHIP_RUBY_SHOW        = true;
    SPACESHIP_RUST_SHOW        = true;
    SPACESHIP_HASKELL_SHOW     = true;
    SPACESHIP_JULIA_SHOW       = true;
    SPACESHIP_EXEC_TIME_SHOW   = true;
    SPACESHIP_EXEC_TIME_COLOR  = "red";
    SPACESHIP_BATTERY_SHOW     = "always";
    SPACESHIP_VI_MODE_SHOW     = true;
    SPACESHIP_JOBS_SHOW        = true;
    SPACESHIP_EXIT_CODE_SHOW   = true;
  };

  shellAliases = {
    dh      = "dirs -v";
    ls      = "ls --color=auto";
    ll      = "ls -lrthF";
    lla     = "ll -A";
    lld     = "ll /dev/sd*";
    rm      = "rm --preserve-root -i";
    mrannex = "mr -c ~/data/annex/mrconfig";
  };
}

