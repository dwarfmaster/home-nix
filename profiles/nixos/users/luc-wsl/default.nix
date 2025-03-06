{ pkgs, ... }:

{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    profiles = {
      system = {
        android.enable = false;
        direnv.enable = true;
        encryption.enable = true;
        network.enable = true;
        templates.enable = false;
        xdg.enable = true;
      };

      interface = {
        brightness.enable = false;
        bspwm.enable = false;
        eww.enable = false;
        korrvigs.enable = true;
        locking.enable = false;
        picom.enable = false;
        visualisation.enable = true;
        x11.enable = false;
        xmonad.enable = false;
      };

      programs = {
        audio.enable = false;
        blender.enable = false;
        cheat.enable = true;
        documents.enable = false;
        drawing.enable = false;
        engineering.enable = false;
        firefox.enable = false;
        games.enable = false;
        git-annex.enable = true;
        kitty.enable = false;
        maps.enable = false;
        messaging.enable = false;
        multimedia.enable = false;
        neovim.enable = true;
        passwords.enable = false;
        vim.enable = true;
      };

      data = {
        accounting.enable = false;
        book.enable = false;
        calendar.enable = false;
        contacts.enable = false;
        mail.enable = false;
        music.enable = false;
        nextcloud.enable = false;
        papers.enable = false;
        photos.enable = false;
        wiki.enable = false;
      };

      languages = {
        agda.enable = false;
        andromeda.enable = false;
        asciidoc.enable = false;
        coq.enable = false;
        cpp.enable = true;
        dedukti.enable = false;
        elpi.enable = false;
        go.enable = false;
        haskell.enable = true;
        idris.enable = false;
        java.enable = false;
        julia.enable = true;
        latex.enable = true;
        lean.enable = true;
        lua.enable = true;
        nix.enable = true;
        nushell.enable = false;
        ocaml.enable = false;
        prolog.enable = false;
        python3.enable = true;
        ruby.enable = false;
        rust.enable = true;
        sql.enable = true;
        tools.enable = true;
        why3.enable = false;
      };
    };
  };
}
