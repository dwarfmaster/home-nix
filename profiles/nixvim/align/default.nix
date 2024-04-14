{ pkgs, ... }:

{
  extraPlugins = [
    pkgs.vimPlugins.vim-easy-align
  ];

  keymaps = [
    {
      key = "ga";
      action = "<Plug>(EasyAlign)";
      options.desc = "Easy align";
    }
  ];
}
