{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    imports = [
      # System
      ../../../user/system/android
      ../../../user/system/direnv
      ../../../user/system/encryption
      ../../../user/system/xdg

      # Interface
      ../../../user/interface/x11
      ../../../user/interface/visualisation
      ../../../user/interface/locking

      # Programs
      ../../../user/programs/firefox
      ../../../user/programs/chromium
      ../../../user/programs/emacs
      ../../../user/programs/audio
      ../../../user/programs/blender
      ../../../user/programs/documents
      ../../../user/programs/drawing
      ../../../user/programs/engineering
      ../../../user/programs/git-annex
      ../../../user/programs/maps
      ../../../user/programs/messaging
      ../../../user/programs/multimedia
      ../../../user/programs/passwords

      # Data
      ../../../user/data/mail
      ../../../user/data/photos
      ../../../user/data/music
      ../../../user/data/book
      ../../../user/data/papers
      ../../../user/data/accounting

      # Languages
      ../../../user/languages/tools
      ../../../user/languages/coq
      ../../../user/languages/agda
      ../../../user/languages/idris
      ../../../user/languages/why3
      ../../../user/languages/cpp
      ../../../user/languages/julia
      ../../../user/languages/python3
      ../../../user/languages/haskell
      ../../../user/languages/ocaml
      ../../../user/languages/latex
    ];
  };
}
