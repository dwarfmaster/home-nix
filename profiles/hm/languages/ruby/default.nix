{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    # LSP for ruby
    pkgs.rubyPackages.solargraph
  ];
  programs.nixvim = {
    plugins.lsp.enabledServers = [{ 
      name = "solargraph"; 
      extraOptions = {};
    }];
  };
}
