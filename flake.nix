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
        url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-21.05";
        inputs.nixpkgs.follows = "unstable";
        inputs.nixpkgs-21_05.follows = "nixos";
      };
      nur = {
        url = "github:nix-community/NUR";
      };
      lean4 = {
        url = "github:leanprover/lean4";
        inputs.nixpkgs.follows = "unstable";
      };
    };

  outputs = inputs@{ self, home, nixos, master, unstable, nur, simple-mailserver, lean4 }:
    let
      # All overlays to apply
      finalOverlays = self.overlays // {
        nur = nur.overlay;
        packages = self: super: {
          lean4 = lean4.defaultPackage.x86_64-linux;
        };
      };
      # Modules to be made available to hosts config
      finalModules = self.nixosModules // {
        home-manager = home.nixosModules.home-manager;
        mailserver   = simple-mailserver.nixosModules.mailserver;
      };
      # HM Modules to be made available to profiles
      finalHMModules = self.hmModules // {
        rycee-base16 = nur-no-pkgs.repos.rycee.hmModules.theme-base16;
      };

    # After this point there is no configuration, only plumbing
      inherit (builtins) attrNames attrValues;
      inherit (nixos) lib;
      inherit (lib) recursiveUpdate;
      utils = import ./lib/utils.nix { inherit lib; };
      inherit (utils) pathsToImportedAttrs readVisible;

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

      nur-no-pkgs = import nur {
        nurpkgs = pkgImport false nixos;
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
          overlayPaths = map fullPath (attrNames (readVisible overlayDir));
        in pathsToImportedAttrs overlayPaths;

      nixosModules =
        let
          modulesDir = ./modules/nixos;
          fullPath = name: modulesDir + "/${name}";
          modulesPaths = map fullPath (attrNames (readVisible modulesDir));
        in pathsToImportedAttrs modulesPaths;

      hmModules =
        let
          modulesDir = ./modules/hm;
          fullPath = name: modulesDir + "/${name}";
          modulesPaths = map fullPath (attrNames (readVisible modulesDir));
        in pathsToImportedAttrs modulesPaths;

      nixosConfigurations =
        import ./hosts (inputs // {
          inherit system;
          inherit lib utils pkgset;
          inherit finalOverlays finalModules finalHMModules;
        });
    };
}
