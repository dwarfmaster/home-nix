{
  pkgs,
  ...
}: {
  home.packages = [ pkgs.korrvigs ];
  programs.nixvim.extraPlugins = [ pkgs.nvim-korrvigs ];
  home.persistence."/persists/luc".directories = [
    ".config/korrvigs"
  ];
}
