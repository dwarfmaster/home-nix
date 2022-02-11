{ fetchFromGitHub
, opam2nix
, ocaml-ng
}:

let
  selection = opam2nix.build {
    ocaml = ocaml-ng.ocamlPackages_4_12.ocaml;
    selection = ./opam-v20.nix;
    src = fetchFromGitHub {
      owner  = "Deducteam";
      repo   = "lambdapi";
      rev    = "2.0.0";
      sha256 = "1pjvyhnq86pkl6lgany25ybyl5b3x3v4p1m7kk631zqrqzk481ms";
    };
  };
in selection.lambdapi
