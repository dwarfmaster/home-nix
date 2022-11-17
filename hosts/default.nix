{ lib
, pkgs
, self
, finalOverlays
, finalModules
}:
let
  inherit (lib) types utils;
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;

  modules = hostName: system:
    let
      sys-pkgs = pkgs system;

      global = {
        _module.args.system = system;

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;

        networking.hostName = hostName;
        nix.nixPath = let path = toString ../.; in
          [
            "nixpkgs=${self.inputs.nixos}"
            "unstable=${self.inputs.unstable}"
            "master=${self.inputs.master}"
            "nixos=${self.inputs.nixos}"
            "nixpkgs-overlays=${path}/overlays"
          ];

        nixpkgs.pkgs = sys-pkgs;

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
