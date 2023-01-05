{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.doom-emacs.config = {
    initModules = {lang = ["nix"];};
    modules.dwarfmaster.nix.config.text = ''
      (after! nix-mode
        (add-hook! 'nix-mode-hook #'lsp!))
    '';
  };

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
