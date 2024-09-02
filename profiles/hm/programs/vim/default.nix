{
  config,
  pkgs,
  lib,
  ...
}: let
  lightline-theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "mike-hearn";
      repo = "base16-vim-lightline";
      rev = "cea11b7d0a407eccf0085feefb5e3be2ec99bdbb";
      sha256 = "0lcxcikkbk03lvsjjjqv22ypl9wpa7zkb6ndbc2gasskckzk3fa9";
    };
  };

  lightline-theme-plugin = pkgs.vimUtils.buildVimPlugin {
    name = "lightline-stylix";
    pname = "lightline-stylix";

    src = lightline-theme;
    dontUnpack = true;

    buildPhase = ''
      install -D $src $out/autoload/lightline/colorscheme/stylix.vim
    '';
  };
in {
  programs.vim = {
    enable = true;
    extraConfig = builtins.readFile ./vimrc;
    plugins = builtins.attrValues {
      inherit
        (pkgs.vimPlugins)
        lightline-vim # Status line
        vim-gitgutter # Display git information in the gutter
        vim-polyglot # Support for many languages
        ;
      inherit
        lightline-theme-plugin
        ;
    };
  };
}
