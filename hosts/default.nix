{ lib
, self
, overlays
, modules
}:
let
  inherit (lib) types;
  inherit (builtins) attrValues removeAttrs;

  host-modules = hostName: system:
    let
      global = {
        profiles.core.enable = lib.mkDefault true;

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.sharedModules = builtins.attrValues modules.hm;

        networking.hostName = hostName;
        nix.nixPath = let path = toString ../.; in
          [
            "nixpkgs=${self.inputs.nixos}"
            "unstable=${self.inputs.unstable}"
            "master=${self.inputs.master}"
            "nixos=${self.inputs.nixos}"
            "nixpkgs-overlays=${path}/overlays"
          ];

        # Will use the nixpkgs from which nixosSystem is called
        nixpkgs = {
          overlays = builtins.attrValues overlays;
          config = {
            allowUnfree = false;
          };
          localSystem.system = system;
        };

        nix.registry = {
          nixos.flake = self.inputs.nixos;
          nixpkgs.flake = self.inputs.nixos;
        };

        system.configurationRevision = lib.mkIf (self ? rev) self.rev;
      };

      local = import "${toString ./.}/${hostName}.nix";
    in
      (attrValues modules.nixos) ++ [ global local ];

  mkHost = hostname: system: {
    modules = host-modules hostname system;
    inherit system;
  };

in {
  tungdil    = mkHost "tungdil"    "x86_64-linux";
  duna       = mkHost "duna"       "x86_64-linux";
  vraccas    = mkHost "vraccas"    "x86_64-linux";
  sharindlar = mkHost "sharindlar" "aarch64-linux";
}
