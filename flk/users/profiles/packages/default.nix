{ pkgs, ... }:

{
  home.packages = with pkgs; [
    #  ____            _                 
    # / ___| _   _ ___| |_ ___ _ __ ___  
    # \___ \| | | / __| __/ _ \ '_ ` _ \ 
    #  ___) | |_| \__ \ ||  __/ | | | | |
    # |____/ \__, |___/\__\___|_| |_| |_|
    #        |___/                       
    # Cryptography
    openssh          # Misc cryptographic utilities
    mosh             # Better SSH for bad connections
    gnupg            # GNU pgp implementation
    gnupg1           # GNU pgp implementation (TODO why is it necessary ?)
    pinentry-curses  # Save password input for gpg
    cryptsetup       # Setup DM encrypted disks

    # Compression
    unrar # RAR decompression
    p7zip # Terminal implementation of 7zip
    unzip # ZIP compression and decompression
    zip

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
    usbutils     # for lsusb
    duc          # Inspect disk usage

    # Utilities
    gparted         # Partition editing
    woeusb          # Create bootable windows USB keys
    stress          # System stress tester
    patchelf        # NixOS utility to make a binary compatible
    postgresql      # SQL database
    ethtool         # Query/control network drivers/hardware settings
    bridge-utils    # Create and manage bridge devices
    binutils        # Misc binary utilites (include objdump, ld and as)
    alsaUtils       # Sound card control
    pavucontrol     # PulseAudio control
    cachix          # Nix binary caches handling
    nixops          # Nix-based deployement


    #  ____  _          _ _
    # / ___|| |__   ___| | |
    # \___ \| '_ \ / _ \ | |
    #  ___) | | | |  __/ | |
    # |____/|_| |_|\___|_|_|
    #
    # Shells and multiplexers
    bash   # GNU shell
    fish   # Shell focused on interaction
    tmux   # Terminal multiplexer and splitter
    abduco # Terminal multiplexer

    # Utilities
    most             # Advanced pager
    jq               # CLI JSON interaction
    tree             # Display directories arborescence
    file             # Misc information about a file
    bc               # Terminal calculator
    telnet           # Unsecure direct download
    sqlite           # SQlite database access and manipulation
    dhall            # Better json
    magic-wormhole   # File sharing
    croc             # Same but better

    # Fonts
    powerline-fonts   # Fonts with icons
    nerdfonts         # Fonts with icons
    iosevka           # Good font
    fira-code         # Good font
    fira-mono
    fira-code-symbols
    font-manager      # Preview fonts

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
    (python3.withPackages (ppkgs: [ ]))
    # julia       # Julia interpreter

    # Haskell
    cabal-install
    cabal2nix
    (haskellPackages.ghcWithHoogle (hpkgs: with hpkgs; [ diagrams ]))

    # Idris
    (idrisPackages.with-packages
      (with idrisPackages; [ lightyear contrib ]))

    # Proof assistants
    coq
    # (why3.withProvers [why3-z3 why3-cvc4 why3-alt-ergo])
    z3
    cvc4

    # C/C++
    gcc       # C/C++ compiler
    gdb       # C/C++ debugger
    ddd       # Graphical frontendfor GDB
    pkgconfig # Library finder

    # OCaml
    ocaml
    opam
    dune_2
    ocamlPackages.utop

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
    autoconf # Makefile generator
    automake # Same

    # Data viewing
    gnuplot     # Plotting programming language
    paraview    # An interface to vtk


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
    gnome3.dconf-editor    # GTK configuration editor
    desktop-file-utils     # Manage desktop files

    # System
    xorg.xev            # X11 event querying
    xorg.xprop          # X11 properties querying
    xclip               # X11 copy-paste from the console
    glxinfo             # OpenGL info
    xorg.xf86videointel # For intel-virtual-output, handling hdmi monitors


    #  ____            _
    # |  _ \  ___  ___(_) __ _ _ __
    # | | | |/ _ \/ __| |/ _` | '_ \
    # | |_| |  __/\__ \ | (_| | | | |
    # |____/ \___||___/_|\__, |_| |_|
    #                    |___/
    # Sound
    audacity # sound editor

    # 2D
    gimp      # scalar image editor
    inkscape  # vectorial image editor
    asymptote # 2D and 3D mathematical drawing
    gv        # PS utilities

    # 3D
    blender    # 3D design and animation
    solvespace # generic CAD
    freecad    # generic CAD

    # Misc
    leocad     # LEGO models designer

    # Maps
    viking     # GPS traces editor

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
    ffmpeg-full                 # Convert any video/audio format to any other
    qpdf                        # Content preserving pdf transformations
    fanficfare                  # Download and convert to epub fanfiction from the web
    python38Packages.youtube-dl # Video downloader
    pandoc                      # Markdown converter

    # Editors
    bvi # Hexadecimal editor

    # Office
    gnumeric                     # Graphical spreadsheet
    abiword                      # Graphical text editor
    sc-im                        # NCurses spreadsheet
    links                        # Terminal web browser
    graphviz                     # Graph drawing
    python27Packages.dot2tex     # Convert graphviz graphs to LaTeX
    texlive.combined.scheme-full # All of texlive (including LaTeX and ConTEXt)
    libreoffice

    # Communication
    discord          # Audio and chat
    signal-desktop   # Access signal
    fractal          # Chat client for matrix
    nheko            # Idem
    zoom-us          # Video meeting

    # Misc
    pass                         # CLI password manager
    gitAndTools.git-annex        # Files manager
    haskellPackages.hledger      # Accounting software
    chromium
    exif                         # Query image metadata
    exiv2                        # Query and edit image metadata
  ];
}
