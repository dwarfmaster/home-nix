{ pkgs, self, recdata, ... }:

let lib = import ./lib.nix; in
let packages = with pkgs; [
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
  cryptsetup       # Setup DM encrypted disks

  # Compression
  unrar # RAR decompression
  p7zip # Terminal implementation of 7zip
  unzip # ZIP compression and decompression

  # Libraries
  librsvg # SVG rendering library (TODO why is it necessary ?)
  libmtp  # MTP support
  mtpfs   # Mount MTP devices

  # Informations
  cpufrequtils # Information about the cpu
  lm_sensors   # Access sensors like CPU temperature
  pciutils     # Misc pci informations (contains lspci)
  lsof         # List users of a device
  wireshark    # Networks packets reading

  # Utilities
  gparted      # Partition editing
  woeusb       # Create bootable windows USB keys
  stress       # System stress tester
  patchelf     # NixOS utility to make a binary compatible
  postgresql   # SQL database
  ethtool      # Query/control network drivers/hardware settings
  bridge-utils # Create and manage bridge devices
  binutils     # Misc binary utilites (include objdump, ld and as)
  alsaUtils    # Sound card control


  #  ____  _          _ _ 
  # / ___|| |__   ___| | |
  # \___ \| '_ \ / _ \ | |
  #  ___) | | | |  __/ | |
  # |____/|_| |_|\___|_|_|
  #                       
  # Shells and multiplexers
  dash   # Minimalistic shell
  bash   # GNU shell
  fish   # Shell focused on interaction
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

  # Misc
  figlet           # Font ASCII art
  cmatrix          # Cascade of letters, matrix style
  cowsay           # ASCII art of a cow speaking
  asciiquarium     # ASCII art aquarium
  sl               # ASCII art train
  beep             # Play sounds on the terminal
  powerline-fonts  # Fonts with icons
  nerdfonts        # Fonts with icons
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
  ghostscript # PostScript interpreter

  # Haskell
  cabal-install
  (haskellPackages.ghcWithHoogle
    (hpkgs: builtins.concatLists (builtins.map (f: f hpkgs)
                                               (lib.defAccess [ "haskellPackages" ] recdata [ ])))    
  )

  # Idris
  idris
  idrisPackages.lightyear

  # Stage M1
  openjdk8               # JAVA implementation
  ccl                    # Clozure CL, common lisp implementation
  lispPackages.quicklisp # Library manager for common lisp

  # PDRE
  opam
  ocaml
  gmp
  jbuilder
  obuild
  ocamlPackages.camlp4
  ocamlPackages.ansiterminal
  ocamlPackages.cppo
  ocamlPackages.ocsigen_deriving
  ocamlPackages.qcheck
  ocamlPackages.findlib
  ocamlPackages.apron
  ocamlPackages.utop

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


  #  ____            _    _              
  # |  _ \  ___  ___| | _| |_ ___  _ __  
  # | | | |/ _ \/ __| |/ / __/ _ \| '_ \ 
  # | |_| |  __/\__ \   <| || (_) | |_) |
  # |____/ \___||___/_|\_\\__\___/| .__/ 
  #                               |_|    
  # Window manager
  dmenu                  # Application launcher

  # Utilities
  redshift               # Color shift with the time of the day
  lighthouse             # Application launcher and multi purpose menu
  i3lock                 # Screen locker
  imlibsetroot           # Background picture setter

  # System
  xorg.xev    # X11 event querying
  xorg.xprop  # X11 properties querying
  xclip       # X11 copy-paste from the console
  glxinfo     # OpenGL info


  #  ____            _             
  # |  _ \  ___  ___(_) __ _ _ __  
  # | | | |/ _ \/ __| |/ _` | '_ \ 
  # | |_| |  __/\__ \ | (_| | | | |
  # |____/ \___||___/_|\__, |_| |_|
  #                    |___/       
  # Sound
  audacity # sound editor

  # 2D
  gimp     # scalar image editor
  inkscape # vectorial image editor

  # 3D
  blender    # 3D design and animation
  solvespace # generic CAD
  freecad    # generic CAD

  # Misc
  leocad # LEGO models designer

  #  __  __       _ _   _                    _ _       
  # |  \/  |_   _| | |_(_)_ __ ___   ___  __| (_) __ _ 
  # | |\/| | | | | | __| | '_ ` _ \ / _ \/ _` | |/ _` |
  # | |  | | |_| | | |_| | | | | | |  __/ (_| | | (_| |
  # |_|  |_|\__,_|_|\__|_|_| |_| |_|\___|\__,_|_|\__,_|
  #                                                    
  # Readers
  mpv     # video player
  okular  # heavy featureful pdf reader
  zathura # lightweight pdf reader
  sxiv    # images reader
  calibre # epub reader

  # Conversion
  imagemagick                 # Convert any image format to any other
  qpdf                        # Content preserving pdf transformations
  fanficfare                  # Download and convert to epub fanfiction from the web
  python35Packages.youtube-dl # Video downloader
  pandoc                      # Markdown converter

  # Editors
  bvi # Hexadecimal editor

  # Office
  gnumeric                 # Graphical spreadsheet
  abiword                  # Graphical text editor
  sc-im                    # NCurses spreadsheet
  links                    # Terminal web browser
  graphviz                 # Graph drawing
  python27Packages.dot2tex # Convert graphviz graphs to LaTeX

  # Misc
  remind                # CLI advanced calendar
  wyrd                  # NCurses interface to remind (TODO deprecated ?)
  pass                  # CLI password manager
  gitAndTools.git-annex # Files manager
  newsboat              # RSS feed manager


  #   ____                           
  #  / ___| __ _ _ __ ___   ___  ___ 
  # | |  _ / _` | '_ ` _ \ / _ \/ __|
  # | |_| | (_| | | | | | |  __/\__ \
  #  \____|\__,_|_| |_| |_|\___||___/
  #                                  
  mupen64plus
  zsnes
  superTuxKart
  superTux
  armagetronad
  gltron
  (dwarf-fortress.override {
    enableDFHack = true; # does not work as of now 2016-09-13
    themes       = {};
    theme        = null;
  })
  dwarf-therapist
  wesnoth
  hedgewars
  rogue
]; in

packages ++ lib.defAccess [ "packages" ] recdata [ ]

