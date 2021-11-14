{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;
  inherit (pkgs) fetchFromGitHub;
in {
  cpp = fetchFromGitHub {
    owner  = "qpeq";
    repo   = "cpp_cmake_boilerplate";
    rev    = "8833eea5112521fe6eb5b4dca6d5584e010e86ef";
    sha256 = "1kbbsyn8rs1nqxxgc7ycvib8slvg1cp6mprcivjx0lxqw0r5pmxs";
  };
}
