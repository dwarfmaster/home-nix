{
  pkgs,
  ...
}: {
  plugins.lsp.enable = true;
  # TODO enable again when migrating to 23.11
  # plugins.neogen.enable = true;

  extraPlugins = [ pkgs.vimPlugins.lspsaga-nvim ];
  # TODO get outline floating window working
  extraConfigLua = ''
    require('lspsaga').setup({
      lightbulb = {
        enable = true;
        sign = false;
        virtual_text = true;
      };
      symbols_in_winbar = { enable = true; };
      outline = {
        auto_preview = false;
        auto_close = true;
      };
    })
  '';

  keymaps = let
    picker = key: name: desc: {
      inherit key;
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      options.desc = desc;
    };
    saga = key: action: desc: {
      inherit key;
      action = "<cmd>Lspsaga ${action}<cr>";
      options.desc = desc;
    };
  in [
    (picker "gd" "lsp_definitions" "Jump to definition")
    (picker "gD" "lsp_implementations" "Jump to implementation")
    (picker "gt" "lsp_type_definition" "Jump to type definition")
    (saga "gI" "finder imp" "Jump to implementation")
    (saga "]e" "diagnostic_jump_next" "Next diagnostic")
    (saga "[e" "diagnostic_jump_prev" "Prev diagnostic")
    # "<leader>cl".desc = "lsp";
    {
      key = "<leader>cli";
      action = "<cmd>LspInfo<cr>";
      options.desc = "Info";
    }
    {
      key = "<leader>clS";
      action = "<cmd>LspStop<cr>";
      options.desc = "Stop";
    }
    {
      key = "<leader>cls";
      action = "<cmd>LspStart<cr>";
      options.desc = "Start";
    }
    {
      key = "<leader>clR";
      action = "<cmd>LspRestart<cr>";
      options.desc = "Restart";
    }
    (picker "<leader>cr" "lsp_references" "References to current symbol")
    (picker "<leader>cs" "lsp_document_symbols" "List symbols in document")
    (saga "<leader>co" "outline" "Show outline")
    (saga "<leader>cS" "finder" "List symbols in workspace")
    (saga "<leader>ck" "hover_doc" "Doc")
    (saga "<leader>cp" "peek_definition" "Preview definition")
    (saga "<leader>cP" "peek_type_definition" "Preview type definition")
    (saga "<leader>cg" "show_line_diagnostics" "Diagnostics on line")
    (picker "<leader>cG" "diagnostics" "Diagnostics")
    (saga "<leader>ca" "code_action" "Code actions")
    (saga "<leader>cR" "rename" "Rename")
    # "<leader>cd".desc = "diagnostics";
    (saga "<leader>cdd" "show_line_diagnostics" "At line")
    (saga "<leader>cdc" "show_cursor_diagnostics" "At cursor")
    (picker "<leader>cdf" "diagnostics" "All file")
    (saga "<leader>td" "toggle_virtual_text" "Inline diagnostics")
    {
      key = "<leader>cn";
      action = "<cmd>Neogen<cr>";
      options.desc = "Generate annotations";
    }
  ];
}
