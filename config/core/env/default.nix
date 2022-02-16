{ config, pkgs, ... }:

let
  inherit (pkgs) unfree;
in {
  home.packages = builtins.attrValues {
    # Cryptography
    inherit (pkgs)
      openssh          # Misc cryptographic utilities
      mosh             # Better SSH for bad connections
      openssl          # Generic cryptography tool
      signify          # ED25519 signature system
      ;

    # Compression
    inherit (pkgs)
      p7zip        # Terminal implementation of 7zip
      unzip        # ZIP decompression
      zip          # ZIP compression and decompression
      ;
    inherit (unfree)
      unrar        # RAR decompression
      ;

    # Libraries
    inherit (pkgs)
      librsvg # SVG rendering library (TODO why is it necessary ?)
      libmtp  # MTP support
      mtpfs   # Mount MTP devices
      ;

    # Informations
    inherit (pkgs)
      # Not available on aarch64
      # cpufrequtils # Information about the cpu
      lm_sensors   # Access sensors like CPU temperature
      pciutils     # Misc pci informations (contains lspci)
      lsof         # List users of a device
      usbutils     # for lsusb
      duc          # Inspect disk usage
      ;

    # Utilities
    inherit (pkgs)
      bc               # Terminal calculator
      binutils         # Misc binary utilites (include objdump, ld and as)
      bridge-utils     # Create and manage bridge devices
      cachix           # Nix binary caches handling
      croc             # Better file sharing
      dhall            # Better json
      ethtool          # Query/control network drivers/hardware settings
      exif             # Query image metadata
      exiv2            # Query and edit image metadata
      file             # Misc information about a file
      jq               # CLI JSON interaction
      #links            # Terminal web browser
      magic-wormhole   # File sharing
      mkpasswd         # Tool to encode passwords for config files
      most             # Advanced pager
      patchelf         # NixOS utility to make a binary compatible
      perl             # Scripting language
      sqlite           # SQlite database access and manipulation
      stress           # System stress tester
      telnet           # Unsecure direct download
      tree             # Display directories arborescence
      # TODO doesn't work on aarch64
      # woeusb           # Create bootable windows USB keys
      zbar             # Tool to work with QR code
      ;

    # Useless but fun
    inherit (pkgs)
      figlet           # Font ASCII art
      #cmatrix          # Cascade of letters, matrix style
      #cowsay           # ASCII art of a cow speaking
      #asciiquarium     # ASCII art aquarium
      #sl               # ASCII art train
      #beep             # Play sounds on the terminal
      ;

    # TODO sort
    #  wireshark    # Networks packets reading
    #  gparted         # Partition editing
    #  alsaUtils       # Sound card control
    #  pavucontrol     # PulseAudio control
    #  postgresql      # SQL database
  };
}
