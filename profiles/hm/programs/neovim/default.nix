{ config, pkgs, lib, ... }:

{
  imports = [ ./core.nix ./keys.nix ./which-keys.nix ];

  home.packages = [
    pkgs.unstable.neovide
    # pkgs.neovim
    pkgs.fzy
  ];

  programs.nixvim = {
    enable = true;
    extraPlugins = [];
    extraConfigLua = ''
    '';
    options = {
      number = true;
      relativenumber = true;
    };

    # Colorscheme
    # TODO enable support for colorscheme not based on name but on colors
    # Maybe user nvim-base16
    colorschemes.base16 = {
      enable = true;
      useTruecolor = true;
      colorscheme = lib.toLower config.colorScheme.name;
    };

    plugins = {
      which-key = {
        enable = true;
        operators = { "gc" = "Comments"; };
        plugins.presets.g = false;
        labels = {
          "<cr>" = "RET";
        };
        icons.separator = ":=";
        popup.window = {
          border = "none";
          margin.left = 2;
          margin.right = 2;
          margin.bottom = 2;
          blend = 50;
        };
        popup.layout.align = "center";

        bindings = {
          n = {
            "<leader>" =
              { name = "Top-level";
                bindings = {
                  b = { lua = "print(\"bar\")"; description = "bar"; };
                };
                subs = {
                  l.subs = {
                    "u".bindings = {
                      "a" = { lua = "print(\"foo\")"; description = "foo"; };
                      "m" = { cmd = "messages"; description = "messages"; };
                    };
                  };
                };
              };
          };
        };
      };
    };
  };
}
