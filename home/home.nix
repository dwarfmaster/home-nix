{ pkgs, ... } @ args:

let content = import ./main.nix args; in
let lib     = import ../lib/lib.nix; in

# Iterate 2 times should be enough
let iterated = lib.iterate 3 content { }; in
lib.removeAttrs [ "modules" "recdata" ] iterated

