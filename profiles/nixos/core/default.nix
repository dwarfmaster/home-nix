{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./env
    ./locale
    ./nix
    ./shells
  ];

  # Docker support
  virtualisation.docker.enable = true;

  security.protectKernelImage = true;
  services.earlyoom.enable = true;

  users.mutableUsers = false;

  # TODO remove references to python 2, since it reached end of life
  nixpkgs.config.permittedInsecurePackages = [
    "python-2.7.18.6"
    "openssl-1.1.1u"
  ];
}
