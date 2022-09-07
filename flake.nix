{
  description = "A highly structured configuration database.";

  inputs =
    {
      master.url = "nixpkgs/master";
      unstable.url = "nixpkgs/nixos-unstable";
      nixos.url = "nixpkgs/release-22.05";
      wayland = {
        url = "github:nix-community/nixpkgs-wayland";
        inputs.nixpkgs.follows = "nixos";
        inputs.master.follows = "master";
      };
      home = {
        url = "github:nix-community/home-manager/release-22.05";
        inputs.nixpkgs.follows = "nixos";
      };
      nur.url = "github:nix-community/NUR";
      flake-utils.url = "github:numtide/flake-utils";
      nixos-hardware.url = "github:NixOS/nixos-hardware";

      simple-mailserver = {
        url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-22.05";
        inputs.nixpkgs.follows = "unstable";
        inputs.nixpkgs-22_05.follows = "nixos";
      };
      django.url = "github:pnmadelaine/django-nixos/main";
      imacs = {
        url = "github:TWal/imacs";
        inputs.nixpkgs.follows = "nixos";
        inputs.django-nixos.follows = "django";
      };
      colors.url = "github:Misterio77/nix-colors";

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
      nixvim = {
        url = "github:pta2002/nixvim";
        inputs.nixpkgs.follows = "unstable";
      };
      nix-autobahn.url = "github:Lassulus/nix-autobahn";
      arkenfox.url = "github:dwarfmaster/arkenfox-nixos";
    };

  outputs = inputs@{ self, home, nixos, master, unstable, wayland, nur, flake-utils,
                     nixos-hardware, simple-mailserver, django, imacs, colors,
                     lean4, opam2nix, emacs-overlay, nix-doom-emacs, nixvim,
                     nix-autobahn, arkenfox }:
    let
      # All overlays to apply
      finalOverlays = self.overlays // {
        nur = nur.overlay;
        arkenfox = arkenfox.overlay;
        # wayland = wayland.overlay;
        packages = re: super: {
          lean4 = lean4.defaultPackage.x86_64-linux;
          opam2nix = opam2nix.defaultPackage.x86_64-linux;
          nix-autobahn = nix-autobahn.defaultPackage.x86_64-linux;
          nix-colors = colors.colorSchemes;
        };
      };
      # Modules to be made available to hosts config
      finalModules = self.nixosModules // {
        mailserver   = simple-mailserver.nixosModules.mailserver;
        home-manager = home.nixosModules.home-manager;
        imacs        = imacs.nixosModules.imacs;
      };
      # HM Modules to be made available to profiles
      finalHMModules = system: self.hmModules // {
        rycee-base16   = (nur-no-pkgs system).repos.rycee.hmModules.theme-base16;
        nix-doom-emacs = nix-doom-emacs.hmModule;
        arkenfox       = arkenfox.hmModules.default;
        nixvim         = nixvim.homeManagerModules.nixvim;
        colors         = colors.homeManagerModules.colorScheme;
      };
      # All attributes to add to lib
      finalLib = self.lib // {
        hardware   = nixos-hardware.nixosModules;
        system     = flake-utils.lib.system;
        allSystems = flake-utils.lib.allSystems;
        arkenfox   = arkenfox.lib.arkenfox;
      };
      # Supported system
      supportedSystems = builtins.attrValues {
        inherit (flake-utils.lib.system)
          x86_64-linux
          aarch64-linux;
      };

      # After this point there is no configuration, only plumbing
      inherit (builtins) attrNames attrValues;
      inherit (nixos) lib;
      utils = import ./lib/utils.nix { inherit lib; };
      inherit (utils) pathsToImportedAttrs readVisible importProfiles;
      eachSupportedSystem = f: builtins.listToAttrs
        (map (system: lib.nameValuePair system (f system)) supportedSystems);

      nixosProfiles = importProfiles ./profiles/nixos;
      hmProfiles = importProfiles ./profiles/hm;

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

      packages = system: import ./packages {
        pkgs = import nixos {
          inherit system;
          config = { allowUnfree = false; };
          overlays = [ (self: super: pkgs-variants system) ];
        };
        inherit lib utils;
      };

      pkgs = system: import nixos {
        inherit system;
        config = { allowUnfree = false; };
        overlays =
          attrValues finalOverlays ++ [
            (self: super: pkgs-variants system)
            (self: super: {
              lib = super.lib.extend
                (final: prev: {
                  currentSystem = system;
                  profiles = hmProfiles;
                } // finalLib);
            })
            (self: super: packages system)
          ];
      };

      nur-no-pkgs = system: import nur {
        nurpkgs = pkgImport system false nixos;
      };

      hmConfigurations =
        system: import ./users {
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
      packages = eachSupportedSystem packages;

      lib = import ./lib { inherit lib pkgs; };

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (attrNames (readVisible overlayDir));
        in pathsToImportedAttrs overlayPaths;

      legacyPackages = eachSupportedSystem pkgs;

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
        builtins.mapAttrs (_: config: let
          plib = (pkgs config.system).lib;
        in lib.nixosSystem {
          inherit (config) system modules;
          lib = plib.extend (final: prev: {
            hmConfigurations = (hmConfigurations config.system).configurations;
            profiles = nixosProfiles;
          });
        }) hosts;

      hmConfigurations = eachSupportedSystem (system:
        builtins.removeAttrs (hmConfigurations system) [ "configurations" ]);
    };
}
