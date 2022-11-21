{ lib, config, ... }:
{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    home.persistence."/persists/luc" = {
      allowOther = true;
      directories = [
        "data"
        "downloads"
        "repos"
        "nextcloud"
        ".gnupg"
        ".ssh"
        ".password-store"
        ".local/share/direnv"
        # Doom emacs needs its directory as long as the nix compilation is broken
        ".emacs.d"
        # Firefox
        # TODO be more precise about what's saved
        ".mozilla/firefox/Personal"
        ".mozilla/firefox/Thesis"
        ".mozilla/firefox/Media"
        ".mozilla/firefox/Private"
        ".mozilla/firefox/Config"
        ".mozilla/firefox/Shopping"
        ".mozilla/firefox/Secure"
        # Neovim
        ".local/share/nvim"
        ".cache/nvim"
        # Misc software
        ".opam"
        ".cargo"
        ".julia"
        ".cabal"
        ".directory_history"
        ".logseq"
        ".config/discord"
        ".local/share/xorg"
        ".local/share/signal-cli"
        ".local/share/zsh"
        ".cache/bash"
        ".local/share/Nextcloud"
        ".config/Nextcloud"
      ];
      files = [
        ".zdirs"
        ".ghc/ghci_history"
      ];
    };

    systemd.user.tmpfiles.rules = [
      "L /home/luc/wiki - - - - /home/luc/data/wiki"
      "L /home/luc/social - - - - /home/luc/data/social"
      "L /home/luc/div - - - - /home/luc/data/div"
      "L /home/luc/workflow - - - - /home/luc/data/workflow"
      "L /home/luc/papers - - - - /home/luc/data/papers"
      "L /home/luc/projects - - - - /home/luc/data/projects"
      "L /home/luc/photos - - - - /home/luc/data/photos"
      "L /home/luc/mail - - - - /home/luc/data/mail"
      "L /home/luc/contact - - - - /home/luc/data/contact"
      "L /home/luc/calendar - - - - /home/luc/data/calendar"
    ];
  };
}
