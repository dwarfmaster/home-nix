{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Profiles
    ./config.nix
  ];

  home.packages = [
    pkgs.unstable.neovide
    pkgs.fzy
  ];

  programs.nixvim = {
    enable = true;
    wrapRc = false;
    # files."ftplugin/nix.vim".extraConfigVim = ''
    #   noremap gg :echom "test"
    # '';
    # files."ftplugin/ocaml.lua".extraConfigVim = ''
    #   noremap gg :echom "test ocaml"
    # '';
    # files."ftplugin/tmp.lua".extraConfigLua = ''
    #   function MyTest()
    #   end
    # '';
    # files."ftdetect/tmp.lua".autoCmd = [
    #   {
    #     event = [ "BufRead" "BufNewFile" ];
    #     pattern = [ "*.temp" ];
    #     command = "set ft=tmp";
    #   }
    # ];
  };

  services.korrvigs = {
    constants.nvim = "${config.programs.neovim.package}/bin/nvim";
    extraModules.nvim = ./nvim.pl;
  };
}
