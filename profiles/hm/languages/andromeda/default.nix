{ config, lib, pkgs, ... }:

let
  andromedaPkg = { stdenv, fetchFromGitHub, ocaml, ocamlPackages }:
    stdenv.mkDerivation {
      pname = "andromeda";
      version = "2.0";

      src = fetchFromGitHub {
        owner  = "Andromedans";
        repo   = "andromeda";
        rev    = "761b0fd07cab5cbcf68a06e79b7f27301826ca82";
        sha256 = "0qn0ccdpawz1ilb7jijmncwp0zmn8qqsmrxyqdxfm4nlqxy3zi5x";
      };

      patches = [ ./make.patch ];

      buildInputs = [
        ocaml
      ] ++ builtins.attrValues {
        inherit (ocamlPackages)
          ocamlbuild
          findlib
          menhir
          menhirLib
          sedlex
        ;
      };

      installPhase = ''
        make install PREFIX=$out
      '';
    };
  andromeda = pkgs.callPackage andromedaPkg { };
in {
  # TODO fix
  # home.packages = [ andromeda ];
}
