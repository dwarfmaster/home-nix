{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins.lsp.enabledServers = [
      {
        name = "nil_ls";
        extraOptions = {
          formatting.command = ["alejandra" "--quiet"];
        };
      }
    ];
  };

  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      nil
      alejandra # Formatter
      ;
  };
}
