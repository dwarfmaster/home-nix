{ home
, lib
, nixos
, master
, unstable
, pkgs
, self
, system
, utils
, finalModules
, finalHMModules
, ...
}:
let
  inherit (lib) types;
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      modules =
        let
          global = {
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

          local = import "${toString ./.}/${hostName}.nix";

          hm-default = {
            options = {
              home-manager-default-modules = lib.mkOption {
                type = types.attrs;
                description = "An attr-set of home-manager modules to include in every configuration";
                readOnly = true;
                internal = true;
                visible = false;
              };
            };

            config = {
              home-manager-default-modules = finalHMModules;
            };
          };

        in
          (attrValues finalModules) ++ [ hm-default global local ];

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
