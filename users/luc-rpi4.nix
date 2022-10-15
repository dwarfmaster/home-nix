{ lib, ... }:

{
  imports = [ ./luc-common.nix ] ++ (builtins.attrValues {
    # System
    inherit (lib.profiles.system)
      direnv
      encryption
      xdg
      templates
    ;

    # Interface
    inherit (lib.profiles.interface)
      #x11
      #xmonad
      #visualisation
      #locking
      #brightness
    ;


    # Programs
    inherit (lib.profiles.programs)
      #firefox
      #chromium
      #emacs
      #audio
      #documents
      #drawing
      #git-annex
      #messaging
      #multimedia
      #passwords
      vim
      neovim
    ;

    # Data
    inherit (lib.profiles.data)
      #mail
      #photos
      #music
      #book
      #papers
      #accounting
      #feeds
      #wiki
      #calendar
    ;

    # Languages
    inherit (lib.profiles.languages)
      tools
      #coq
      #lean
      #andromeda
      #agda
      #idris
      #why3
      #cpp
      #julia
      #python3
      #haskell
      #ocaml
      #latex
      #dedukti
    ;

  });
}
