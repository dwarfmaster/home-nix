{
  pkgs,
  ...
}: {
  programs.nixvim = {
    lsp.servers.nil_ls.enable = true;
  };

  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      nil
      alejandra # Formatter
      ;
  };
}
