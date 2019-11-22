general:

let
  pkgs = general.pkgs.main;
in {
  home.file.".xmonad".source = ./config;
  home.file.".xmonad".recursive = true;

  packages        = with pkgs.haskellPackages; [ xmonad xmobar ];
  haskellPackages = [ (hpkgs: with hpkgs; [ xmonad xmobar xmonad-extras xmonad-contrib ]) ];
}

