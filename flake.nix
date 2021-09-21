{
  description = "A highly structured configuration database.";

  inputs =
    {
      master.url = "nixpkgs/master";
      unstable.url = "nixpkgs/nixos-unstable";
      nixos.url = "nixpkgs/release-21.05";
      home = {
        url = "github:nix-community/home-manager/release-21.05";
        inputs.nixpkgs.follows = "nixos";
      };
      simple-mailserver = {
        url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
        inputs.nixpkgs.follows = "unstable";
        inputs.nixpkgs-21_05.follows = "nixos";
      };
    };

  outputs = inputs@{ self, home, nixos, master, unstable, simple-mailserver }:
    let
      # All overlays to apply
      finalOverlays = self.overlays // {
      };
      # Modules to be made available to hosts config
      finalModules = self.nixosModules // {
        home-manager = home.nixosModules.nixosModules.home-manager;
        mailserver   = simple-mailserver.nixosModules.mailserver;
      };
      # HM Modules to be made available to profiles
      finalHMModules = self.hmModules // {
      };

    in
    # After this point there is no configuration, only plumbing
    let
      inherit (builtins) attrNames attrValues readDir;
      inherit (nixos) lib;
      inherit (lib) recursiveUpdate;
      utils = import ./lib/utils.nix { inherit lib; };
      inherit (utils) pathsToImportedAttrs;

      system = "x86_64-linux";

      pkgImport = unfree: pkgs:
        import pkgs {
          inherit system;
          overlays = attrValues finalOverlays;
          config = { allowUnfree = unfree; };
        };

      pkgset = {
        master = pkgImport false master;
        master-unfree = pkgImport true master;
        unstable = pkgImport false unstable;
        unstable-unfree = pkgImport true unstable;
        pkgs = pkgImport false nixos;
        unfree = pkgImport true nixos;
      };

    in {
      checks."${system}" = {
        # TODO
      };

      hydraJobs = {
        # TODO;
      };

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (attrNames (readDir overlayDir));
        in pathsToImportedAttrs overlayPaths;

      nixosModules =
        let
          modulesDir = ./modules/nixos;
          fullPath = name: modulesDir + "/${name}";
          modulesPaths = map fullPath (attrNames (readDir modulesDir));
        in pathsToImportedAttrs modulesPaths;

      hmModules = {
        # TODO
      };

      nixosConfigurations = {
        # TODO
      };
    };
}
