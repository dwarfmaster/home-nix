{lib, ...}: {
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    profiles = {
      system = {
        android.enable = true;
        direnv.enable = true;
        encryption.enable = true;
        network.enable = true;
        templates.enable = true;
        xdg.enable = true;
      };

      interface = {
        brightness.enable = true;
        bspwm.enable = false;
        eww.enable = true;
        korrvigs.enable = true;
        locking.enable = true;
        picom.enable = true;
        visualisation.enable = true;
        x11.enable = true;
        xmonad.enable = true;
      };

      programs = {
        audio.enable = true;
        blender.enable = true;
        cheat.enable = true;
        documents.enable = true;
        drawing.enable = false;
        emacs.enable = true;
        engineering.enable = false;
        firefox.enable = true;
        games.enable = true;
        git-annex.enable = true;
        kitty.enable = true;
        maps.enable = true;
        messaging.enable = true;
        multimedia.enable = true;
        neovim.enable = true;
        passwords.enable = true;
        vim.enable = true;
      };

      data = {
        accounting.enable = true;
        book.enable = true;
        calendar.enable = true;
        contacts.enable = true;
        feeds.enable = true;
        mail.enable = true;
        music.enable = true;
        nextcloud.enable = true;
        papers.enable = true;
        photos.enable = true;
        wiki.enable = true;
      };

      languages = {
        agda.enable = true;
        andromeda.enable = true;
        asciidoc.enable = true;
        coq.enable = true;
        cpp.enable = true;
        dedukti.enable = true;
        elpi.enable = true;
        go.enable = true;
        haskell.enable = true;
        idris.enable = true;
        java.enable = true;
        julia.enable = true;
        latex.enable = true;
        lean.enable = true;
        lua.enable = true;
        nix.enable = true;
        norg.enable = true;
        ocaml.enable = true;
        prolog.enable = true;
        python3.enable = true;
        ruby.enable = true;
        rust.enable = true;
        tools.enable = true;
        why3.enable = true;
      };
    };
  };
}
