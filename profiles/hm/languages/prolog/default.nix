{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.swi-prolog];
  programs.nixvim = {
    lsp.servers.prolog_ls.enable = true;
  };
}
