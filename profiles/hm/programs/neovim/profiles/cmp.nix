{ config, pkgs, lsp, ... }:

{
  # TODO more complete setup
  programs.nixvim = {
    extraPlugins = [
      pkgs.vimPlugins.luasnip
      pkgs.vimPlugins.cmp_luasnip
      pkgs.vimPlugins.lspkind-nvim
    ];
    plugins.nvim-cmp = {
      enable = true;
      mappingPresets = [ "insert" "cmdline" ];
      snippet.expand = ''
        function(args)
          require('luasnip').lsp_expand(args.body)
        end
      '';
      sources = [
        { name = "nvim_lsp"; }
        { name = "buffer"; }
        { name = "dictionnary"; }
        { name = "calc"; }
        { name = "path"; }
        { name = "luasnip"; }
      ];
      mapping = {
        "<Tab>" = {
          modes = [ "i" "s" ];
          action = ''
            function(fallback)
              if cmp.visible then
                cmp.select_next_item()
              elseif luasnip.expandable() then
                luasnip.expand()
              elseif luasnip.expand_or_jumpable() then 
                luasnip.expand_or_jump()
              elseif check_backspace() then 
                fallback()
              else
                fallback()
              end
            end
          '';
        };
        "S-<Tab>" = {
          modes = [ "i" "s" ];
          action = ''
            function(fallback)
              if cmp.visible() then 
                cmp.select_prev_item() 
              elseif luasnip.jumpable(-1) then 
                luasnip.jump(-1)
              else 
                fallback()
              end
            end
          '';
        };
        "<Down>" = {
          modes = [ "i" ];
          action = ''
            function(fallback)
              fallback()
            end
          '';
        };
        "<Up>" = {
          modes = [ "i" ];
          action = ''
            function(fallback)
              fallback()
            end
          '';
        };
      };
      # TODO improve format
      formatting = {
        fields = [ "kind" "abbr" "menu" ];
        format = ''
          require('lspkind').cmp_format({
            mode = "symbol_text",
            maxwidth = 50,
            menu = ({
              nvim_lsp = "[LSP]",
              buffer = "[BUF]",
              dictionnary = "[DIC]",
              calc = "[CALC]",
              path = "[PTH]",
              luasnip = "[SNIP]",
            })
          })
        '';
      };
    };
  };
}
