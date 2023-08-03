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
  programs.doom-emacs.config = {
    initModules.lang = ["ocaml"];
    modules.lang.mlg = {
      config.text = ''
        (add-to-list 'auto-mode-alist '("\\.mlg$" . tuareg-mode) t)
      '';
    };
  };
  programs.nixvim = {
    plugins.lsp.enabledServers = ["ocamllsp"];
  };
}
