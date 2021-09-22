{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    imports = [
      # System
      ../../../user/system/packages

      # Interface
      ../../../user/interface/x11

      # Programs
      ../../../user/programs/web
      ../../../user/programs/emacs

      # Data
      ../../../user/data/mail
      ../../../user/data/photos
      ../../../user/data/music
      ../../../user/data/book
      ../../../user/data/papers
      ../../../user/data/accounting

      # Languages
      ../../../user/languages/coq
      ../../../user/languages/agda
      ../../../user/languages/idris
      ../../../user/languages/why3
    ];
  };
}
