{ home
, lib
, nixos
, master
, unstable
, pkgs
, self
, system
, finalModules
, finalHMModules
, ...
}:
let
  inherit (lib) types utils;
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;

  modules = hostName:
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
          nixpkgs.flake = nixos;
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
      };
    in
      (attrValues finalModules) ++ [ hm-default global local ];

  hosts = recImport {
    dir = ./.;
    _import = modules;
  };
in
hosts
