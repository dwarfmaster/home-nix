{ pkgs, self, recdata, ... }:

let

  lib = import ../lib/lib.nix;

in let packages = with pkgs; [
  #  ____            _                 
  # / ___| _   _ ___| |_ ___ _ __ ___  
  # \___ \| | | / __| __/ _ \ '_ ` _ \ 
  #  ___) | |_| \__ \ ||  __/ | | | | |
  # |____/ \__, |___/\__\___|_| |_| |_|
  #        |___/                       
  # Cryptography
  openssh          # Misc cryptographic utilities
  gnupg            # GNU pgp implementation
  gnupg1           # GNU pgp implementation (TODO why is it necessary ?)
  pinentry_ncurses # Save password input for gpg

  # Compression
  unrar # RAR decompression
  p7zip # Terminal implementation of 7zip
  unzip # ZIP compression and decompression

  # Libraries
  librsvg # SVG rendering library (TODO why is it necessary ?)

  # Informations
  cpufrequtils # Information about the cpu
  lm_sensors   # Access sensors like CPU temperature
  pciutils     # Misc pci informations (contains lspci)
  lsof         # List users of a device
  wireshark    # Networks packets reading

  # Utilities
  gparted      # Partition editing
  stress       # System stress tester
  binutils     # Misc binary utilites (include objdump, ld and as)


  #  ____  _          _ _ 
  # / ___|| |__   ___| | |
  # \___ \| '_ \ / _ \ | |
  #  ___) | | | |  __/ | |
  # |____/|_| |_|\___|_|_|
  #                       
  # Shells and multiplexers
  tmux   # Terminal multiplexer and splitter
  abduco # Terminal multiplexer
  dvtm   # Terminal splitter

  # Utilities
  most   # Advanced pager
  jq     # CLI JSON interaction
  tree   # Display directories arborescence
  file   # Misc information about a file
  bc     # Terminal calculator
  telnet # Unsecure direct download
  sqlite # SQlite database access and manipulation
  htop   # Display system information
  gotop  # A variant of htop

  # Misc
  figlet           # Font ASCII art
  cmatrix          # Cascade of letters, matrix style
  cowsay           # ASCII art of a cow speaking
  asciiquarium     # ASCII art aquarium
  sl               # ASCII art train
  beep             # Play sounds on the terminal
  spaceship-prompt # Oh-my-zsh theme

  #  ____                                                _             
  # |  _ \ _ __ ___   __ _ _ __ __ _ _ __ ___  _ __ ___ (_)_ __   __ _ 
  # | |_) | '__/ _ \ / _` | '__/ _` | '_ ` _ \| '_ ` _ \| | '_ \ / _` |
  # |  __/| | | (_) | (_| | | | (_| | | | | | | | | | | | | | | | (_| |
  # |_|   |_|  \___/ \__, |_|  \__,_|_| |_| |_|_| |_| |_|_|_| |_|\__, |
  #                  |___/                                       |___/ 
  # Scripting
  perl        # Perl interpreter
  python2     # Python 2 interpreter
  python3     # Python 3 interperter
  ruby        # Ruby interpreter
  julia       # Julia interpreter

  # Haskell
  cabal-install
  cabal2nix
  (haskellPackages.ghcWithHoogle
    (hpkgs: builtins.concatLists (builtins.map (f: f hpkgs)
                                               (lib.defAccess [ "haskellPackages" ] recdata [ ])))    
  )

  # Idris
  idris
  idrisPackages.lightyear

  # C/C++
  gcc       # C/C++ compiler
  gdb       # C/C++ debugger
  ddd       # Graphical frontendfor GDB
  pkgconfig # Library finder

  # Tools
  pijul    # VCS based on sound theory
  gnumake  # Simple generic purpose build system
  ninja    # Simple generic purpose build system
  cmake    # Generic purpose build system
  cloc     # Count code lines
  valgrind # Generic purpose debugger
  gnum4    # Macro preprocessor
  mr       # Multiple repository management
  ctags    # Objects indexer for many languages
  gradle   # Build system for java
  mercurial


  #  __  __       _ _   _                    _ _       
  # |  \/  |_   _| | |_(_)_ __ ___   ___  __| (_) __ _ 
  # | |\/| | | | | | __| | '_ ` _ \ / _ \/ _` | |/ _` |
  # | |  | | |_| | | |_| | | | | | |  __/ (_| | | (_| |
  # |_|  |_|\__,_|_|\__|_|_| |_| |_|\___|\__,_|_|\__,_|
  #                                                    
  # Conversion
  imagemagick                 # Convert any image format to any other
  qpdf                        # Content preserving pdf transformations
  inkscape

  # Editors
  bvi # Hexadecimal editor

  # Office
  sc-im                    # NCurses spreadsheet
  links                    # Terminal web browser
  graphviz                 # Graph drawing
  gnuplot                  # Scientific plotting
  asymptote
  gephi                    # Interactive graph visualizer
  python27Packages.dot2tex # Convert graphviz graphs to LaTeX
  zathura
  xpdf
  texlive.combined.scheme-full

]; in

packages ++ lib.defAccess [ "packages" ] recdata [ ]

