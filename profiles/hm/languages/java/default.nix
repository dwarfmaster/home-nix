{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.jdk11];

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "java";
          args = ["meghanada"];
        }
      ];
    };
  };
}
