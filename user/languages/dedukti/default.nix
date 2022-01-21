{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs;

  # TODO fix lambdapi package
  lambdapi_v20_pkg = pkgs.callPackage ./lambdapi20.nix { };
  lambdapi_v20 = pkgs.writeShellScriptBin "lambdapi-v2.0" ''
    ${lambdapi_v20_pkg}/bin/lambdapi
  '';
  lambdapi_v21 = pkgs.callPackage ./lambdapi21.nix { };

in {
  home.packages = [
    # lambdapi_v20
    # lambdapi_v21
  ];
  programs.doom = {
    modules.lang.dedukti = {
      config.source = ./config.el;
      packages.text = ''
        (package! lambdapi-mode)
      '';
    };
  };

  # To install lambdapi with opam, I need zlib
  pkgconfig.enable = true;
  pkgconfig.path = [
    "${pkgs.zlib.dev}/lib/pkgconfig"
  ];
}
