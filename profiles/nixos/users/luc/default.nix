{ lib, ... }:

{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    profiles = {
      system = {
        android.enable = true;
        direnv.enable = true;
        encryption.enable = true;
        xdg.enable = true;
        templates.enable = true;
        network.enable = true;
      };

      interface = {
        x11.enable = true;
        xmonad.enable = true;
        visualisation.enable = true;
        locking.enable = true;
        brightness.enable = true;
      };

      programs = {
        firefox.enable = true;
        emacs.enable = true;
        audio.enable = true;
        blender.enable = true;
        documents.enable = true;
        drawing.enable = false;
        engineering.enable = false;
        git-annex.enable = true;
        maps.enable = true;
        messaging.enable = true;
        multimedia.enable = true;
        neovim.enable = true;
        passwords.enable = true;
        games.enable = true;
        cheat.enable = true;
        vim.enable = true;
      };

      data = {
        mail.enable = true;
        photos.enable = true;
        music.enable = true;
        book.enable = true;
        papers.enable = true;
        accounting.enable = true;
        feeds.enable = true;
        wiki.enable = true;
        calendar.enable = true;
        nextcloud.enable = true;
        contacts.enable = true;
      };

      languages = {
        agda.enable = true;
        andromeda.enable = true;
        coq.enable = true;
        cpp.enable = true;
        dedukti.enable = true;
        haskell.enable = true;
        idris.enable = true;
        java.enable = true;
        julia.enable = true;
        latex.enable = true;
        lean.enable = true;
        lua.enable = true;
        nix.enable = true;
        ocaml.enable = true;
        python3.enable = true;
        rust.enable = true;
        tools.enable = true;
        why3.enable = true;
      };
    };
  };
}
