{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = with pkgs; [ zotero ];

  programs.doom = {
    initModules = {
      tools = [ "biblio" ];
    };
    modules.dwarfmaster.papers = {
      config.source = ./config.el;
      nix = {
        pdfreader = "${pkgs.zathura}/bin/zathura";
      };
    };
  };
}
