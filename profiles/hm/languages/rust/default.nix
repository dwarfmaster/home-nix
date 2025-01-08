{
  config,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins.lsp.servers.rust_analyzer = {
      enable = true;
      installCargo = false;
      installRustc = false;
    };
  };
}
