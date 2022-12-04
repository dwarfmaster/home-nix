{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins.lsp.enabledServers = ["prolog_ls"];
  };
}
