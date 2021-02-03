{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    imports = [
      ../profiles/packages

      # Desktop
      ../profiles/x11
      ../profiles/web

      # Workflow
      ../profiles/emacs
      ../profiles/logic
      ../profiles/photos
      ../profiles/music
      ../profiles/book
    ];
  };
}
