general:

let
  pkgs = general.pkgs.main;
in with pkgs; {
  fzf-bibtex = callPackage ./fzf-bibtex.nix { };
}

