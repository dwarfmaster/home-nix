{
  fetchFromGitHub,
  opam2nix,
  ocaml-ng,
}: let
  selection = opam2nix.build {
    ocaml = ocaml-ng.ocamlPackages_4_12.ocaml;
    selection = ./opam-v21.nix;
    src = fetchFromGitHub {
      owner = "Deducteam";
      repo = "lambdapi";
      rev = "2.1.0";
      sha256 = "026vp7h5i6yqvafap9n1g3sh0a3zz8pgbxy4nkhnfg7spdr29svm";
    };
  };
in
  selection.lambdapi
