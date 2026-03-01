{
  pkgs,
  lib,
  ...
}: {
  home.persistence."/persists/luc".directories = [
    ".config/korrvigs"
  ];

  programs.nixvim = {
    plugins.render-markdown = {
      enable = true;
      settings = {
        file_types = [ "markdown" "korrvigs-note" ];
        completions.lsp.enabled = true;
        dash.enabled = false;
        checkbox.enabled = false;
        link.enabled = false;
      };
    };

    extraPlugins = [ pkgs.nvim-korrvigs pkgs.vimPlugins.vim-markdown ];
    extraConfigLua = "require('korrvigs').setup()";
    keymaps = [
      { key = "<leader>nn";
        action.__raw = "function() require('korrvigs').jump_to_note() end";
        options.desc = "Jump to korrvigs note";
      }
      # TODO load only in korrvigs-note filetypes
      { key = "<leader> a";
        action.__raw = "function() require('korrvigs').attach_file('/home/luc/downloads/') end";
        options.desc = "Attach file to korrvigs note";
      }
      { key = "<leader> l";
        action.__raw = "function() require('korrvigs').insert_link() end";
        options.desc = "Edit/insert link";
      }
    ];
  };
}
