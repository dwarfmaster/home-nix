{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins.lsp.servers.rust-analyzer.enable = true;
  };
}
