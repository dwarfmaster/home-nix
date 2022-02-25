{ config, lib, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner  = "TWal";
    repo   = "imacs";
    rev    = "79ac6857769b98706f4ec90a6d6b2659d86ffb6a";
    sha256 = "1589s3dd1xam9pr83gl1igwizi5f0ajbckalwwnvnbgplf5zy5xd";
  };
in {
  services.django = {
    enable = true;
    servers.imacs = {
      keysFile = ./secrets.sh;
      root = "${src}";
      settings = "imacs.settings.nix";
    };
  };
}
