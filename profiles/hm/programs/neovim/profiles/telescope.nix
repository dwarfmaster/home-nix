{ config, pkgs, lib, ... }:

{
  programs.nixvim = {
    plugins.telescope = {
      enable = true;
      extensions = {
        frecency.enable = true;
        fzy-native.enable = true;
      };
    };
    plugins.which-key.bindings.n = let
      picker = name: desc:
        { lua = "require'telescope.builtin'.${name}{}"; description = desc; };
    in {
      "<leader>" = {
        bindings = {
          "/" = picker "live_grep" "Search text";
          "*" = picker "grep_string" "Search text under cursor";
          "," = picker "buffers" "Search buffers";
          ":" = picker "command_history" "Search command history";
        };
        subs = {
          "f".bindings = {
            "f" = picker "find_files" "Search files";
            "r" = picker "oldfiles" "Find recent";
          };
          "b".bindings = {
            "j" = picker "loclist" "Locations";
            "t" = picker "current_buffer_tags" "Search tags";
            "s" = picker "current_buffer_fuzzy_find" "Fuzzy find";
          };
          "v".bindings = {
            "f" = picker "git_files" "Added files";
          };
          "s".bindings = {
            "s" = picker "resume" "Resume previous search";
            "h" = picker "search_history" "Search search history";
            "t" = picker "tags" "Search tags";
            "c" = picker "command_history" "Search command history";
            "m" = picker "marks" "Search marks";
            "r" = picker "registers" "Search registers";
          };
          "h".bindings = {
            "h" = picker "hep_tags" "Search help tags";
            "c" = picker "commands" "List commands";
            "m" = picker "man_pages" "Search man pages";
            "o" = picker "options" "List options";
            "a" = picker "autocommands" "List autocommands";
            "k" = picker "keymaps" "List keymaps";
            "F" = picker "filetypes" "List available filetypes";
            "H" = picker "highlights" "List available highlights";
          };
          "j".bindings = {
            "l" = picker "jumplist" "Jumplist";
          };
        };
      };
    };
  };
}
