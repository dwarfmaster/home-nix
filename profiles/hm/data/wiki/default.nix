{
  config,
  lib,
  pkgs,
  ...
}: {
  services.korrvigs = {
    # Broken due to openssl-1.1.1u deprecation
    # enable = true;
    dataDir = "/home/luc/downloads/wiki";
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "org";
          args = ["roam2" "pretty" "hugo"];
        }
      ];
    };
    modules.dwarfmaster = {
      org-config.config.source = ./org.el;
      org-attach.config.source = ./attach.el;
      org-links.config.source = ./links.el;
      org-roam = {
        config.source = ./roam.el;
        nix = {
          xdg-cache = "${config.xdg.cacheHome}";
        };
      };
    };
  };

  home.packages = [
    pkgs.hugo # Generate the web wiki
  ];
}
