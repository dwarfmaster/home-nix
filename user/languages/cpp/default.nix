{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      gcc       # C/C++ compiler
      gdb       # C/C++ debugger
      ddd       # Graphical frontendfor GDB
      pkgconfig # Library finder
      ccls      # LSP server for CPP
      ;
  };

  programs.doom = {
    initModules = {
      lang = [ { mod = "cc"; args = [ "lsp" ]; } ];
    };
  };

  programs.cookiecutter.templates = {
    # TODO add CCLS support to template
    cpp = ./cmake;
  };
}
