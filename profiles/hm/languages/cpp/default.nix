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
      pkgconfig # Library finder
      ccls # LSP server for CPP
      bazel # Build-system by google
      bazel-buildtools # buildifier, buildozer and unused_deps
      ;
    inherit
      scan-build # Creates command database
      ;
  };

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "cc";
          args = ["lsp"];
        }
      ];
    };
    modules.lang.bazel = {
      config.source = ./bazel.el;
      packages.text = ''
        (package! bazel)
      '';
      nix = {
        bazel = "${pkgs.bazel}/bin/bazel";
        buildifier = "${pkgs.bazel-buildtools}/bin/buildifier";
        buildozer = "${pkgs.bazel-buildtools}/bin/buildozer";
      };
    };
  };

  programs.cookiecutter.templates = {
    # TODO add CCLS support to template
    cpp = ./cmake;
  };
}
