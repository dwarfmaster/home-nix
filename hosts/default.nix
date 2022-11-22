{ lib
, pkgs
, self
, finalOverlays
, finalModules
, finalHMModules
}:
let
  inherit (lib) types utils;
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;

  modules = hostName: system:
    let
      global = {
        profiles.core.enable = lib.mkDefault true;

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.sharedModules = builtins.attrValues (finalHMModules system);

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
          overlays = builtins.attrValues finalOverlays;
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
      (attrValues finalModules) ++ [ global local ];

  mkHost = hostname: system: {
    modules = modules hostname system;
    inherit system;
    pkgs = pkgs system;
  };

in {
  tungdil    = mkHost "tungdil"    "x86_64-linux";
  duna       = mkHost "duna"       "x86_64-linux";
  vraccas    = mkHost "vraccas"    "x86_64-linux";
  sharindlar = mkHost "sharindlar" "aarch64-linux";
}
