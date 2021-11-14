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
    cpp = pkgs.fetchFromGitHub {
      owner  = "qpeq";
      repo   = "cpp_cmake_boilerplate";
      rev    = "8833eea5112521fe6eb5b4dca6d5584e010e86ef";
      sha256 = "1kbbsyn8rs1nqxxgc7ycvib8slvg1cp6mprcivjx0lxqw0r5pmxs";
    };
  };
}
