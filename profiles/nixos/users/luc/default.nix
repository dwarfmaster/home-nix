{ lib, ... }:

{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    imports = builtins.attrValues {
      # System
      inherit (lib.hm.system)
        android
        direnv
        encryption
        xdg
        templates
        network
      ;


      # Interface
      inherit (lib.hm.interface)
        x11
        xmonad
        visualisation
        locking
        brightness
      ;


      # Programs
      inherit (lib.hm.programs)
        firefox
        emacs
        audio
        blender
        documents
        # drawing
        # engineering
        git-annex
        maps
        messaging
        multimedia
        neovim
        passwords
        games
        cheat
        vim
      ;

      # Data
      inherit (lib.hm.data)
        mail
        photos
        music
        book
        papers
        accounting
        feeds
        wiki
        calendar
        nextcloud
        contacts
      ;

      # Languages
      inherit (lib.hm.languages)
        agda
        andromeda
        coq
        cpp
        dedukti
        haskell
        idris
        java
        julia
        latex
        lean
        lua
        nix
        ocaml
        python3
        rust
        tools
        why3
      ;
    };

    hardware.specs = {
      cores = lib.mkDefault 6;
      threads = lib.mkDefault 12;
    };
  };
}
