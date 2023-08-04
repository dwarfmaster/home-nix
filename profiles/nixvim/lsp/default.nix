{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.lsp = {
    enable = true;
  };
  plugins.lspsaga = {
    enable = true;
    signs.use = true;
    borderStyle = "rounded";
  };

  maps.normal = let
    picker = name: desc: {
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      inherit desc;
    };
    saga = action: desc: {
      action = "<cmd>Lspsaga ${action}<cr>";
      inherit desc;
    };
  in {
    "gd" = picker "lsp_definitions" "Jump to definition";
    "gD" = picker "lsp_implementations" "Jump to implementation";
    "gt" = picker "lsp_type_definition" "Jump to type definition";
    "]e" = saga "diagnostic_jump_next" "Next diagnostic";
    "[e" = saga "diagnostic_jump_prev" "Prev diagnostic";
    "<leader>cl".desc = "lsp";
    "<leader>cli" = {
      action = "<cmd>LspInfo<cr>";
      desc = "Info";
    };
    "<leader>clS" = {
      action = "<cmd>LspStop<cr>";
      desc = "Stop";
    };
    "<leader>cls" = {
      action = "<cmd>LspStart<cr>";
      desc = "Start";
    };
    "<leader>clR" = {
      action = "<cmd>LspRestart<cr>";
      desc = "Restart";
    };
    "<leader>cr" = picker "lsp_references" "References to current symbol";
    "<leader>cs" = picker "lsp_document_symbols" "List symbols in document";
    "<leader>cS" = picker "lsp_workspace_symbols" "List symbols in workspace";
    "<leader>ck" = saga "hover_doc" "Doc";
    "<leader>cp" = saga "preview_definition" "Preview definition";
    "<leader>cg" = saga "show_cursor_diagnostics" "Diagnostics at cursor";
    "<leader>cG" = picker "diagnostics" "Diagnostics";
    "<leader>ca" = saga "code_action" "Code actions";
    "<leader>cR" = saga "rename" "Rename";
    "<leader>cd".desc = "diagnostics";
    "<leader>cdd" = saga "show_line_diagnostics" "At line";
    "<leader>cdc" = saga "show_cursor_diagnostics" "At cursor";
    "<leader>cdf" = picker "diagnostics" "All file";
    "<leader>td" = saga "toggle_virtual_text" "Inline diagnostics";
  };
}
