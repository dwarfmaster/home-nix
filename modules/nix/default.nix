{ config, pkgs, ... }:

{
  nix = {
    useSandbox = true;
    binaryCaches = [
      "https://cache.nixos.org"
      "https://hydra.iohk.io"
      "https://cache.dhall-lang.org"
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
    # Users that can import closures
    trustedUsers = [ "root" "@wheel" ];
  };
}
