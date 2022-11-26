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

  plugins.which-key.bindings.n = let
    picker = name: desc: {
      lua = "require'telescope.builtin'.${name}{}";
      description = desc;
    };
    saga = action: desc: {
      cmd = "Lspsaga ${action}";
      description = desc;
    };
  in {
    "g".bindings = {
      "d" = picker "lsp_definitions" "Jump to definition";
      "D" = picker "lsp_implementations" "Jump to implementation";
      "t" = picker "lsp_type_definition" "Jump to type definition";
    };
    "]".bindings."e" = saga "diagnostic_jump_next" "Next diagnostic";
    "[".bindings."e" = saga "diagnostic_jump_prev" "Prev diagnostic";
    "<leader>".subs."c".subs."l".name = "lsp";
    "<leader>".subs."c".subs."l".bindings = {
      "i" = {
        cmd = "LspInfo";
        description = "Info";
      };
      "S" = {
        cmd = "LspStop";
        description = "Stop";
      };
      "s" = {
        cmd = "LspStart";
        description = "Start";
      };
      "R" = {
        cmd = "LspRestart";
        description = "Restart";
      };
    };
    "<leader>".subs."c".bindings = {
      "r" = picker "lsp_references" "References to current symbol";
      "s" = picker "lsp_document_symbols" "List symbols in document";
      "S" = picker "lsp_workspace_symbols" "List symbols in workspace";
      "k" = saga "hover_doc" "Doc";
      "p" = saga "preview_definition" "Preview definition";
      "g" = saga "show_cursor_diagnostics" "Diagnostics at cursor";
      "G" = picker "diagnostics" "Diagnostics";
      "a" = saga "code_action" "Code actions";
      "R" = saga "rename" "Rename";
    };
    "<leader>".subs."c".subs."d".name = "diagnostics";
    "<leader>".subs."c".subs."d".bindings = {
      "d" = saga "show_line_diagnostics" "At line";
      "c" = saga "show_cursor_diagnostics" "At cursor";
      "f" = picker "diagnostics" "All file";
    };
    "<leader>".subs."t".bindings."d" = saga "toggle_virtual_text" "Inline diagnostics";
  };
  # TODO setup float terminal
}
