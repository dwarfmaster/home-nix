{ ... }:

let

  home-manager-pkgs = import <home-manager> { };

in {
  packages = [ home-manager-pkgs.home-manager ];

  # programs.home-manager = {
  #   enable = true;
  #   path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  # };
}

