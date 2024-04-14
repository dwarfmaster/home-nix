{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      ocaml
      opam
      dune_2
      ocamlformat
      ;
    inherit
      (pkgs.ocamlPackages)
      utop
      merlin
      ocp-indent
      zarith
      ;
  };
  programs.nixvim = {
    plugins.lsp.enabledServers = [{ 
      name = "ocamllsp"; 
      extraOptions = {};
    }];
  };
}
