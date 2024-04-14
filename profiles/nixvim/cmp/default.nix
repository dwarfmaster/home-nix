{
  config,
  pkgs,
  ...
}: {
  # TODO luasnip
  # TODO cmdline still not working
  extraPlugins = [
    pkgs.vimPlugins.luasnip
    pkgs.vimPlugins.cmp_luasnip
    pkgs.vimPlugins.lspkind-nvim
  ];
  plugins.cmp = {
    enable = true;
    # mappingPresets = ["cmdline"];
    settings = {
      preselect = "None";
      # TODO
      #   function(args)
      #     require('luasnip').lsp_expand(args.body)
      #   end
      # '';
      snippet.expand = "luasnip";
      sources = [
        {name = "nvim_lsp";}
        {name = "buffer";}
        {name = "dictionnary";}
        {name = "calc";}
        {name = "path";}
        {name = "luasnip";}
      ];
      mapping = let
        tabAction = ''
          function(fallback)
            if cmp.visible() then
              if #cmp.get_entries() == 1 then
                cmp.confirm({ select = true })
              else
                cmp.select_next_item()
              end
            -- TODO setup luasnip
            -- elseif luasnip.expandable() then
            --   luasnip.expand()
            -- elseif luasnip.expand_or_jumpable() then
            --   luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end
        '';
      in {
        "<Tab>" = ''cmp.mapping({
            i = ${tabAction},
            s = ${tabAction},
            c = function(_)
              if cmp.visible() then
                if #cmp.get_entries() == 1 then
                  cmp.confirm({ select = true })
                else
                  cmp.select_next_item()
                end
              else
                cmp.complete()
                if #cmp.get_entries() == 1 then
                  cmp.config({ select = true })
                end
              end
            end
          })
        '';
        "S-<Tab>" = ''
          function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            -- elseif luasnip.jumpable(-1) then
            --   luasnip.jump(-1)
            else
              fallback()
            end
          end
        '';
        # If entry is selected, return selects it, otherwise inserts a new line
        "<CR>" = ''
          cmp.mapping({
            i = function(fallback)
              if cmp.visible() and cmp.get_active_entry() then
                cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
              else
                fallback()
              end
            end,
            s = cmp.mapping.confirm({ select = true }),
            c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
          })
        '';
      };
      formatting = {
        fields = ["kind" "abbr" "menu"];
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
  extraConfigLuaPost = ''
    do
      local cmp = require('cmp')
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'nvim_lua' },
          { name = 'cmdline' },
          { name = 'path' },
        },
      })
    end
  '';
}
