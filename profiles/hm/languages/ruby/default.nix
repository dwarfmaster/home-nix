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
    lsp.servers.solargraph.enable = true;
  };
}
