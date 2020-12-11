args:

let
  unfree-config = { allowUnfree = true; };
in rec {
  main        = nixpkgs.nixos-20-09;
  unfree-main = nixpkgs-unfree.nixos-20-09;
  nixpkgs        = import ./nixpkgs { };
  nixpkgs-unfree = import ./nixpkgs { config = unfree-config; };
  hies           = import ./hie     { };
  doom-emacs     = import ./nix-doom-emacs { pkgs = main; };
}

