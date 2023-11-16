{
  lib,
  self,
  overlays,
  modules,
  inputs,
}: let
  inherit (lib) types;
  inherit (builtins) attrValues removeAttrs;

  host-modules = hostName: let
    global = {pkgs, lib, ...}: {
      profiles.core.enable = lib.mkDefault true;

      # Add flake inputs to system closure to prevent garbage collection
      environment.etc."flake-inputs".text =
        lib.concatStringsSep "\n"
          (lib.mapAttrsToList (k: v: "${k} -> ${v}") inputs);

      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.sharedModules =
        (builtins.attrValues modules.hm)
        ++ [
          {
            profiles.core.enable = true;
            programs.nixvim = {...}: {
              imports = builtins.attrValues modules.nixvim;
            };
          }
        ];

      networking.hostName = hostName;
      nix.nixPath = let
        path = toString ../.;
      in [
        "nixpkgs=${self.inputs.nixos}"
        "stable=${self.inputs.stable}"
        "master=${self.inputs.master}"
        "nixos=${self.inputs.nixos}"
        "nixpkgs-overlays=${path}/overlays"
      ];

      # Will use the nixpkgs from which nixosSystem is called
      nixpkgs = {
        overlays =
          builtins.attrValues overlays;
        config = {
          allowUnfree = false;
        };
      };

      nix.registry = {
        nixos.flake = self.inputs.nixos;
        nixpkgs.flake = self.inputs.nixos;
      };

      system.configurationRevision = lib.mkIf (self ? rev) self.rev;
    };

    local = import "${toString ./.}/${hostName}.nix";
  in
    (attrValues modules.nixos) ++ [global local];

  mkHost = hostname: {
    modules = host-modules hostname;
  };
in {
  tungdil = mkHost "tungdil";
  duna = mkHost "duna";
  vraccas = mkHost "vraccas";
  sharindlar = mkHost "sharindlar";
  helzvog = mkHost "helzvog";
}
