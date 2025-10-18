{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.swi-prolog];
  programs.nixvim = {
    plugins.lsp.enabledServers = [{ 
      name = "prolog_ls"; 
      extraOptions = {};
    }];
  };
}
