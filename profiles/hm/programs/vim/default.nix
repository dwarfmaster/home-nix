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
      rev = "769ab1a87797fa7b510447c4d661b9192c3a7588";
      sha256 = "0kydnm0iqvgp1y5nrrndg69pg17g7yvrh2fbs2ipl438yy2sjs7m";
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
  stylix.targets.vim.enable = true;
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
        lightline-theme-plugin;
    };
  };
}
