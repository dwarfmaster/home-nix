{
  lib,
  fetchFromGitHub,
  ocamlPackages,
  opam2nix,
}:
ocamlPackages.buildOcaml {
  pname = "lambdapi";
}
