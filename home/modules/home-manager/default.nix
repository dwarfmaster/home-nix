args:

let

  pkgs = args.pkgs.main;

in {
  packages = [ pkgs.home-manager ];
}

