{ config, lib, pkgs, ... }:

{
  programs.doom-emacs.config = {
    initModules = { lang = [ "nix" ]; };
    modules.dwarfmaster.nix.config.text = ''
      (after! nix-mode
        (add-hook! 'nix-mode-hook #'lsp!))
    '';
  };

  home.packages = builtins.attrValues {
    inherit (pkgs)
      rnix-lsp
      nixfmt
    ;
  };
}
