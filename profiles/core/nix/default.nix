{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];

    useSandbox = true;
    autoOptimiseStore = true;
    gc.automatic = true;
    optimise.automatic = true;

    binaryCaches = [
      "https://cache.nixos.org"
      "https://hydra.iohk.io"
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes ca-references
      min-free = 536870912
    '';

    allowedUsers = [ "@wheel" ];
    # Users that can import closures
    trustedUsers = [ "root" "@wheel" ];
  };
}