{ config, lib, pkgs, ... }:

{
  imports = [
    ./zsh.nix
    ./bash.nix
  ];

  environment = {
    shellInit = ''
      export STARSHIP_CONFIG=${
        pkgs.writeText "starship.toml"
        (lib.fileContents ./starship.toml)
      }
    '';
  };
}
