{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.telescope = {
    enable = true;
    extensions = {
      frecency.enable = true;
      fzy-native.enable = true;
    };
  };
  maps.normal = let
    picker = name: desc: {
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      desc = desc;
    };
  in {
    "<leader>/" = picker "live_grep" "Search text";
    "<leader>*" = picker "grep_string" "Search text under cursor";
    "<leader>," = picker "buffers" "Search buffers";
    "<leader>:" = picker "command_history" "Search command history";
    "<leader>ff" = picker "find_files" "Search files";
    "<leader>fr" = picker "oldfiles" "Find recent";
    "<leader>bj" = picker "loclist" "Locations";
    "<leader>bt" = picker "current_buffer_tags" "Search tags";
    "<leader>bs" = picker "current_buffer_fuzzy_find" "Fuzzy find";
    "<leader>vf" = picker "git_files" "Added files";
    "<leader>ss" = picker "resume" "Resume previous search";
    "<leader>sh" = picker "search_history" "Search search history";
    "<leader>st" = picker "tags" "Search tags";
    "<leader>sc" = picker "command_history" "Search command history";
    "<leader>sm" = picker "marks" "Search marks";
    "<leader>sr" = picker "registers" "Search registers";
    "<leader>hh" = picker "help_tags" "Search help tags";
    "<leader>hc" = picker "commands" "List commands";
    "<leader>hm" = picker "man_pages" "Search man pages";
    "<leader>ho" = picker "options" "List options";
    "<leader>ha" = picker "autocommands" "List autocommands";
    "<leader>hk" = picker "keymaps" "List keymaps";
    "<leader>hF" = picker "filetypes" "List available filetypes";
    "<leader>hH" = picker "highlights" "List available highlights";
    "<leader>jl" = picker "jumplist" "Jumplist";
  };
}
