{
  config,
  pkgs,
  ...
}: {
  nix = {
    package = pkgs.nixVersions.stable;

    gc.automatic = true;
    optimise.automatic = true;
    settings = {
      sandbox = true;
      auto-optimise-store = true;
      max-jobs = config.hardware.specs.threads;
      system-features =
        ["nixos-test" "benchmark" "big-parallel"]
        ++ (
          if config.hardware.specs.kvm
          then ["kvm"]
          else []
        );

      binary-caches = [
        "https://cache.nixos.org"
      ];

      allowed-users = ["@wheel"];
      # Users that can import closures
      trusted-users = ["root" "@wheel"];
    };

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
      min-free = 536870912
    '';
  };
}
