{ home
, lib
, nixos
, master
, unstable
, pkgset
, self
, system
, utils
, finalModules
, ...
}:
let
  inherit (lib) types;
  inherit (utils) recImport mkPackagesModule;
  inherit (builtins) attrValues removeAttrs;
  inherit (pkgset) pkgs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      modules =
        let
          global = {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            networking.hostName = hostName;
            nix.nixPath = let path = toString ../.; in
              [
                "nixpkgs=${nixos}"
                "unstable=${unstable}"
                "master=${master}"
                "nixos=${nixos}"
                "nixpkgs-overlays=${path}/overlays"
              ];

            nixpkgs = { inherit pkgs; };

            nix.registry = {
              nixos.flake = nixos;
              nixpkgs.flake = master;
            };

            system.configurationRevision = lib.mkIf (self ? rev) self.rev;
          };

          packages = mkPackagesModule pkgset;

          local = import "${toString ./.}/${hostName}.nix";

        in
          (attrValues finalModules) ++ [ packages global local ];

      extraArgs = {
        inherit system utils;
      };
    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in
hosts
