{
  config,
  pkgs,
  ...
}: {
  nix = {
    package = pkgs.nixFlakes;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
    maxJobs = config.hardware.specs.threads;

    useSandbox = true;
    autoOptimiseStore = true;
    gc.automatic = true;
    optimise.automatic = true;

    binaryCaches = [
      "https://cache.nixos.org"
    ];
    binaryCachePublicKeys = [
    ];

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
      min-free = 536870912
    '';

    allowedUsers = ["@wheel"];
    # Users that can import closures
    trustedUsers = ["root" "@wheel"];
  };
}
