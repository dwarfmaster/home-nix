{ pkgs, ... }:

let
  all-hies-gen = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  all-hies = all-hies-gen.selection { selector = p: { inherit (p) ghc864 ghc863; }; };
in {
  "coc.preferences" = {
    timeout = 1000;
    "codeLens.enable" = true;
  };

  languageserver = {
    # C/C++ server
    ccls = {
      command      = "${pkgs.ccls}/bin/ccls";
      filetypes    = [ "c" "cpp" "cc" ];
      # rootPatterns = [ ".ccls" "compile_commands.json" ".git/" ];
      initializationOptions.cache.directory = "/tmp/ccls";
    };

    # Haskell server
    haskell = {
      command = "${all-hies}/bin/hie-wrapper";
      rootPatterns = [
        ".stack.yaml"
        "cabal.config"
        "package.yaml"
        ".hie"
      ];
      filetypes = [ "hs" "lhs" "haskell" ];
      initializationOptions.languageServerHaskell = { };
    };
  };
}
