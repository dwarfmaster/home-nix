{...}: {
  programs.nixvim = {
    lsp.servers.gopls.enable = true;
  };
}
