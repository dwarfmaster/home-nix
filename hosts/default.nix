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
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;
  inherit (pkgset) osPkgs pkgs;

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

            nixpkgs = { pkgs = osPkgs; };

            nix.registry = {
              nixos.flake = nixos;
              nixpkgs.flake = master;
            };

            system.configurationRevision = lib.mkIf (self ? rev) self.rev;
          };

          local = import "${toString ./.}/${hostName}.nix";

        in
        finalModules ++ [ global local ];

      extraArgs = {
        inherit system;
      } // removeAttrs pkgset [ "pkgs" ];
    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in
hosts
