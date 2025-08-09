{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      gcc # C/C++ compiler
      gdb # C/C++ debugger
      ddd # Graphical frontendfor GDB
      pkg-config # Library finder
      ccls # LSP server for CPP
      bazel # Build-system by google
      bazel-buildtools # buildifier, buildozer and unused_deps
      ;
  };

  programs.cookiecutter.templates = {
    # TODO add CCLS support to template
    cpp = ./cmake;
  };
}
