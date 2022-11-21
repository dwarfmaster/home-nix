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

      impermanence.url = "github:nix-community/impermanence";
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
      neovim-nightly = {
        url = "github:nix-community/neovim-nightly-overlay";
        inputs.nixpkgs.follows = "nixos";
      };
      nixvim = {
        url = "github:pta2002/nixvim";
        inputs.nixpkgs.follows = "nixos";
      };
      neorg = {
        url = "github:nvim-neorg/nixpkgs-neorg-overlay";
        inputs.nixpkgs.follows = "nixos";
      };
      nix-autobahn.url = "github:Lassulus/nix-autobahn";
      arkenfox.url = "github:dwarfmaster/arkenfox-nixos";
    };

  outputs = inputs@{ self, home, nixos, master, unstable, wayland, nur, flake-utils,
                     nixos-hardware, impermanence, simple-mailserver, django, imacs, colors,
                     lean4, opam2nix, emacs-overlay, nix-doom-emacs, neovim-nightly, nixvim, neorg,
                     nix-autobahn, arkenfox }:
    let
      # All overlays to apply
      finalOverlays = self.overlays // {
        nur = nur.overlay;
        arkenfox = arkenfox.overlay;
        # neorg = neorg.overlays.default;
        # wayland = wayland.overlay;
        packages = re: super: {
          lean4 = lean4.defaultPackage.x86_64-linux;
          opam2nix = opam2nix.defaultPackage.x86_64-linux;
          nix-autobahn = nix-autobahn.defaultPackage.x86_64-linux;
          nix-colors = colors.colorSchemes;
          neovim-nightly = neovim-nightly.packages.default;
        };
      };
      # Modules to be made available to hosts config
      finalModules = self.nixosModules // {
        inherit libModule;
        mailserver   = simple-mailserver.nixosModules.mailserver;
        home-manager = home.nixosModules.home-manager;
        imacs        = imacs.nixosModules.imacs;
        impermanence = impermanence.nixosModule;
      };
      # HM Modules to be made available to profiles
      finalHMModules = system: self.hmModules // {
        inherit libModule;
        rycee-base16   = (nur-no-pkgs system).repos.rycee.hmModules.theme-base16;
        nix-doom-emacs = nix-doom-emacs.hmModule;
        arkenfox       = arkenfox.hmModules.default;
        nixvim         = nixvim.homeManagerModules.nixvim;
        colors         = colors.homeManagerModules.colorScheme;
        impermanence   = impermanence.nixosModules.home-manager.impermanence;
      };
      # All attributes to add to lib in modules
      extraLib = {
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
      utils = import ./utils.nix { inherit lib; };
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
        inherit lib;
        inherit utils;
      };

      pkgs = system: import nixos {
        inherit system;
        config = { allowUnfree = false; };
        overlays =
          attrValues finalOverlays ++ [
            (self: super: pkgs-variants system)
            (self: super: packages system)
          ];
      };

      nur-no-pkgs = system: import nur {
        nurpkgs = pkgImport system false nixos;
      };

      libModule = { pkgs, lib, ... }@args: {
        lib = utils.recImport { dir = ./lib; _import = base: import "${./lib}/${base}.nix" args; } // {
          inherit utils;
        };
      };

      hosts =
        import ./hosts ({
          inherit self pkgs lib;
          inherit finalOverlays finalModules finalHMModules;
        });

    in {
      packages = eachSupportedSystem packages;

      lib = import ./utils.nix { inherit lib; };

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
        builtins.mapAttrs (_: config: let
          plib = (pkgs config.system).lib;
        in lib.nixosSystem {
          inherit (config) system modules;
          lib = plib.extend (final: prev: {
            profiles = nixosProfiles;
            hm = hmProfiles;
          });
        }) hosts;
    };
}
