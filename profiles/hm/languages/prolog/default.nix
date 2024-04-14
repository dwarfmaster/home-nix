{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins.lsp.enabledServers = [{ 
      name = "prolog_ls"; 
      extraOptions = {};
    }];
  };
}
