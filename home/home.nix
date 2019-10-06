let

  lib = import ../lib/lib.nix;

  build = path: { pkgs, ... }@args:
      lib.removeAttrs
        [ "modules" "recdata" ]
        (lib.iterate 3 (import path args) { }); # 2 iterations would probably be enough

in {

  # Main config for laptops
  main = build ./main.nix;

  # Environment to be copied to my oracle laptop
  oracle = build ./oracle.nix;

}

