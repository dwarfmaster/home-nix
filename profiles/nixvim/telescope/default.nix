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
  keymaps = let
    picker = key: name: desc: {
      inherit key;
      action = "function() require'telescope.builtin'.${name}{} end";
      lua = true;
      options.desc = desc;
    };
  in [
    (picker "<leader>/" "live_grep" "Search text")
    (picker "<leader>*" "grep_string" "Search text under cursor")
    (picker "<leader>," "buffers" "Search buffers")
    (picker "<leader>:" "command_history" "Search command history")
    (picker "<leader>ff" "find_files" "Search files")
    (picker "<leader>fr" "oldfiles" "Find recent")
    (picker "<leader>bj" "loclist" "Locations")
    (picker "<leader>bt" "current_buffer_tags" "Search tags")
    (picker "<leader>bs" "current_buffer_fuzzy_find" "Fuzzy find")
    (picker "<leader>vf" "git_files" "Added files")
    (picker "<leader>ss" "resume" "Resume previous search")
    (picker "<leader>sh" "search_history" "Search search history")
    (picker "<leader>st" "tags" "Search tags")
    (picker "<leader>sc" "command_history" "Search command history")
    (picker "<leader>sm" "marks" "Search marks")
    (picker "<leader>sr" "registers" "Search registers")
    (picker "<leader>hh" "help_tags" "Search help tags")
    (picker "<leader>hc" "commands" "List commands")
    (picker "<leader>hm" "man_pages" "Search man pages")
    (picker "<leader>ho" "options" "List options")
    (picker "<leader>ha" "autocommands" "List autocommands")
    (picker "<leader>hk" "keymaps" "List keymaps")
    (picker "<leader>hF" "filetypes" "List available filetypes")
    (picker "<leader>hH" "highlights" "List available highlights")
    (picker "<leader>jl" "jumplist" "Jumplist")
  ];
}
