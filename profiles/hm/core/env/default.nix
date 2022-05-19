{ config, pkgs, ... }:

let
  inherit (pkgs) unfree;
in {
  home.packages = builtins.attrValues {
    # Cryptography
    inherit (pkgs)
      mkpasswd         # Tool to encode passwords for config files
      mosh             # Better SSH for bad connections
      openssh          # Misc cryptographic utilities
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
      # TODO Not available on aarch64
      # cpufrequtils # Information about the cpu
      duc          # Inspect disk usage
      ethtool      # Query/control network drivers/hardware settings
      lm_sensors   # Access sensors like CPU temperature
      lsof         # List users of a device
      pciutils     # Misc pci informations (contains lspci)
      usbutils     # for lsusb
      wireshark    # Networks packets reading
      ;

    # Utilities
    inherit (pkgs)
      bc               # Terminal calculator
      binutils         # Misc binary utilites (include objdump, ld and as)
      bridge-utils     # Create and manage bridge devices
      cachix           # Nix binary caches handling
      croc             # Better file sharing
      dhall            # Better json
      exif             # Query image metadata
      exiv2            # Query and edit image metadata
      file             # Misc information about a file
      gparted          # Partition editing
      jq               # CLI JSON interaction
      #links            # Terminal web browser
      magic-wormhole   # File sharing
      most             # Advanced pager
      patchelf         # NixOS utility to make a binary compatible
      perl             # Scripting language
      postgresql       # PGSQL database access and manipulation
      sqlite           # SQlite database access and manipulation
      stress           # System stress tester
      telnet           # Unsecure direct download
      tree             # Display directories arborescence
      # TODO doesn't work on aarch64
      # woeusb           # Create bootable windows USB keys
      zbar             # Tool to work with QR code
      reupload         # Easily uploads files on reMarkable
      pv               # cat with progress bar
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
  };
}
