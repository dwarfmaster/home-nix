general:

let
  pkgs = general.pkgs.main;
  all-hies-gen = general.pkgs.hies;
  all-hies = all-hies-gen.selection { selector = p: { inherit (p) ghc865 ghc864 ghc863; }; };
in {
  "coc.preferences" = {
    timeout = 1000;
    "codeLens.enable" = true;
  };

  languageserver = {
    # C/C++ server
    ccls = {
      command      = "${pkgs.ccls}/bin/ccls";
      filetypes    = [ "c" "cpp" ];
      rootPatterns = [ ".ccls" ".git/" ];
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
