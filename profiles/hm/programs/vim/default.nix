{ config, pkgs, lib, ... }:

let
  base16-vim = pkgs.fetchFromGitHub {
    owner = "chriskempson";
    repo = "base16-vim";
    rev = "6a660db238f7cd5dd342982699c016970cbb8a5a";
    sha256 = "1wlzliz0cmyzqghsgl2ag0ikw5wh06gj62smdyyk48qgbkjy6ak4";
  };
  template = "${base16-vim}/templates/default.mustache";

  theme = config.colorScheme;
  colors =
    lib.mapAttrs' (name: v: lib.nameValuePair "${name}-hex" v) theme.colors;
  schemeName = "base16-${theme.slug}";
  colorscheme =
    config.lib.mustache.render "${schemeName}.vim" template
      (colors // {
        scheme-slug = theme.slug;
      });

  lightline-theme = config.lib.mustache.render "base16.vim" ./lightline.vim theme.colors;

  rtp = pkgs.runCommandLocal "vim" {} ''
    mkdir -p $out
    mkdir -p $out/colors
    ln -s ${colorscheme} $out/colors/${schemeName}.vim
    mkdir -p $out/autoload/lightline/colorscheme
    ln -s ${lightline-theme} $out/autoload/lightline/colorscheme/base16.vim
  '';

  vimrc = config.lib.mustache.render "vimrc" ./vimrc {
    extraRtp = "${rtp}";
    colorscheme = schemeName;
  };
in {
  programs.vim = {
    enable      = true;
    # extraConfig = builtins.readFile ./vimrc;
    extraConfig = builtins.readFile "${vimrc}";
    plugins = builtins.attrValues {
      inherit (pkgs.vimPlugins)
        lightline-vim # Status line
        vim-gitgutter # Display git information in the gutter
        vim-polyglot # Support for many languages
      ;
    };
  };
}
