{
  description = "A highly structured configuration database.";

  inputs =
    {
      master.url = "nixpkgs/master";
      unstable.url = "nixpkgs/nixos-unstable";
      nixos.url = "nixpkgs/release-21.11";
      home = {
        url = "github:nix-community/home-manager/release-21.11";
        inputs.nixpkgs.follows = "nixos";
      };
      simple-mailserver = {
        url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-21.11";
        inputs.nixpkgs.follows = "unstable";
        inputs.nixpkgs-21_11.follows = "nixos";
      };
      nur.url = "github:nix-community/NUR";
      nixos-hardware.url = "github:NixOS/nixos-hardware";
      lean4 = {
        url = "github:leanprover/lean4";
        inputs.nixpkgs.follows = "unstable";
      };
      opam2nix = {
        url = "github:dwarfmaster/opam2nix";
        inputs.nixpkgs.follows = "nixos";
      };
      # Forces more recent emacs-overlay, fixes https://github.com/vlaci/nix-doom-emacs/issues/401
      emacs-overlay = {
        url = "github:nix-community/emacs-overlay";
        flake = false;
      };
      nix-doom-emacs = {
        url = "github:nix-community/nix-doom-emacs";
        inputs.nixpkgs.follows = "nixos";
        inputs.emacs-overlay.follows = "emacs-overlay";
      };
      nix-autobahn.url = "github:Lassulus/nix-autobahn";
    };

  outputs = inputs@{ self, home, nixos, master, unstable, nur,
                     nixos-hardware, simple-mailserver, lean4, opam2nix,
                     emacs-overlay, nix-doom-emacs, nix-autobahn }:
    let
      # All overlays to apply
      finalOverlays = self.overlays // {
        nur = nur.overlay;
        packages = self: super: {
          lean4 = lean4.defaultPackage.x86_64-linux;
          opam2nix = opam2nix.defaultPackage.x86_64-linux;
          nix-autobahn = nix-autobahn.defaultPackage.x86_64-linux;
        };
      };
      # Modules to be made available to hosts config
      finalModules = self.nixosModules // {
        mailserver   = simple-mailserver.nixosModules.mailserver;
        home-manager = home.nixosModules.home-manager;
      };
      # HM Modules to be made available to profiles
      finalHMModules = system: self.hmModules // {
        rycee-base16   = (nur-no-pkgs system).repos.rycee.hmModules.theme-base16;
        nix-doom-emacs = nix-doom-emacs.hmModule;
      };

      # After this point there is no configuration, only plumbing
      inherit (builtins) attrNames attrValues;
      utils = import ./lib/utils.nix { lib = nixos.lib; };
      inherit (utils) pathsToImportedAttrs readVisible;

      pkgImport = system: unfree: pkgs:
        import pkgs {
          inherit system;
          overlays = attrValues finalOverlays;
          config = { allowUnfree = unfree; };
        };

      pkgs-variants = system: {
        master = pkgImport system false master;
        master-unfree = pkgImport system true master;
        unstable = pkgImport system false unstable;
        unstable-unfree = pkgImport system true unstable;
        unfree = pkgImport system true nixos;
      };

      pkgs = system: import nixos {
        inherit system;
        config = { allowUnfree = false; };
        overlays =
          attrValues finalOverlays ++ [
            (self: super: pkgs-variants system)
          ];
      };
      lib = nixos.lib.extend
        (final: prev: { 
          inherit utils;
          hardware = nixos-hardware.nixosModules;
        });

      nur-no-pkgs = system: import nur {
        nurpkgs = pkgImport system false nixos;
      };

      hmConfigurations =
        system: import ./users {
          inherit lib;
          finalHMModules = finalHMModules system;
          pkgs = pkgs system;
          inherit (home.lib) homeManagerConfiguration;
        };

      hosts =
        import ./hosts ({
          inherit self pkgs lib;
          inherit finalOverlays finalModules;
        });

    in {
      # TODO extend checks for all architectures
      checks."x86_64-linux" =
        import ./tests (inputs // {
          inherit lib;
          system = "86_64-linux";
          pkgs = pkgs "x86_64-linux";
          inherit (builtins.mapAttrs (_: config: config.modules) hosts);
        });

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
        builtins.mapAttrs (_: config:
          lib.nixosSystem {
            inherit (config) system modules;
            lib = lib.extend (final: prev: {
              hmConfigurations = (hmConfigurations config.system).configurations;
            });
            extraArgs = { inherit (config) system; };
          }) hosts;

      # TODO improve using flake-utils
      hmConfigurations = {
        x86_64-linux  = builtins.removeAttrs (hmConfigurations "x86_64-linux")  [ "configurations" ];
        aarch64-linux = builtins.removeAttrs (hmConfigurations "aarch64-linux") [ "configurations" ];
      };
    };
}
