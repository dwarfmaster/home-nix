{
  imports = [
    ./luc-common.nix

    # System
    ../config/system/android
    ../config/system/direnv
    ../config/system/encryption
    ../config/system/xdg
    ../config/system/templates

    # Interface
    ../config/interface/x11
    ../config/interface/xmonad
    ../config/interface/visualisation
    ../config/interface/locking
    ../config/interface/brightness

    # Programs
    ../config/programs/firefox
    ../config/programs/chromium
    ../config/programs/emacs
    ../config/programs/audio
    ../config/programs/blender
    ../config/programs/documents
    ../config/programs/drawing
    ../config/programs/engineering
    ../config/programs/git-annex
    ../config/programs/maps
    ../config/programs/messaging
    ../config/programs/multimedia
    ../config/programs/passwords

    # Data
    ../config/data/mail
    ../config/data/photos
    ../config/data/music
    ../config/data/book
    ../config/data/papers
    ../config/data/accounting
    ../config/data/feeds
    ../config/data/wiki
    ../config/data/calendar

    # Languages
    ../config/languages/tools
    ../config/languages/coq
    ../config/languages/lean
    ../config/languages/andromeda
    ../config/languages/agda
    ../config/languages/idris
    ../config/languages/why3
    ../config/languages/cpp
    ../config/languages/julia
    ../config/languages/python3
    ../config/languages/haskell
    ../config/languages/ocaml
    ../config/languages/latex
    ../config/languages/dedukti
  ];
}
