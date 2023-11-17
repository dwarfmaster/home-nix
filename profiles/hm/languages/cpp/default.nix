{
  config,
  pkgs,
  ...
}: let
  scan-build = pkgs.python310Packages.callPackage ./scan-build.nix {};
in {
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
    inherit
      scan-build # Creates command database
      ;
  };

  programs.cookiecutter.templates = {
    # TODO add CCLS support to template
    cpp = ./cmake;
  };
}
