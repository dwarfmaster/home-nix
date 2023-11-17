{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = [
    pkgs.ocamlPackages.dedukti
  ];

  # To install lambdapi with opam, I need zlib
  pkgconfig.enable = true;
  pkgconfig.path = [
    "${pkgs.zlib.dev}/lib/pkgconfig"
  ];
}
