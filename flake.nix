{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "nixpkgs/master";
    unstable.url = "nixpkgs/nixos-unstable";
    nixos.url = "nixpkgs/release-22.11";
    home = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixos";
    };
    nur.url = "github:nix-community/NUR";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixos";
    };

    impermanence.url = "github:nix-community/impermanence";
    simple-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-22.11";
      inputs.nixpkgs.follows = "unstable";
      inputs.nixpkgs-22_11.follows = "nixos";
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
    arkenfox.url = "github:dwarfmaster/arkenfox-nixos";
  };

  outputs = inputs @ {
    self,
    home,
    nixos,
    master,
    unstable,
    nur,
    flake-utils,
    nixos-hardware,
    nixos-generators,
    impermanence,
    simple-mailserver,
    django,
    imacs,
    colors,
    lean4,
    opam2nix,
    emacs-overlay,
    nix-doom-emacs,
    neovim-nightly,
    nixvim,
    arkenfox,
  }: let
    # All overlays to apply
    overlays =
      self.overlays
      // {
        nur = nur.overlay;
        arkenfox = arkenfox.overlay;
        neovim-nightly = neovim-nightly.overlay;
        packages = self: super:
          {
            lean4 = lean4.defaultPackage.x86_64-linux;
            opam2nix = opam2nix.defaultPackage.x86_64-linux;
            nix-colors = colors.colorSchemes;
            tree-sitter-make-grammar =
              super.callPackage
              (nixos + "/pkgs/development/tools/parsing/tree-sitter/grammar.nix") {};
          }
          // packages self super;
        variants = self: super: pkgs-variants super.system;
      };
    # Modules to be made available to configs
    modules = {
      nixos =
        self.nixosModules
        // {
          inherit libModule;
          profiles = {...}: {imports = profiles.nixos;};
          nur = nur.nixosModules.nur;
          mailserver = simple-mailserver.nixosModules.mailserver;
          home-manager = home.nixosModules.home-manager;
          imacs = imacs.nixosModules.imacs;
          impermanence = impermanence.nixosModule;
        };
      hm =
        self.hmModules
        // {
          inherit libModule;
          profiles = {...}: {imports = profiles.hm;};
          nur = nur.hmModules.nur;
          rycee-base16 = nur-modules.repos.rycee.hmModules.theme-base16;
          nix-doom-emacs = nix-doom-emacs.hmModule;
          arkenfox = arkenfox.hmModules.default;
          nixvim = nixvim.homeManagerModules.nixvim;
          colors = colors.homeManagerModules.colorScheme;
          impermanence = impermanence.nixosModules.home-manager.impermanence;
        };
      nixvim = self.nixvimModules // {profiles = {...}: {imports = profiles.nixvim;};};
    };
    # All attributes to add to lib in modules
    extraLib = {
      system = flake-utils.lib.system;
      allSystems = flake-utils.lib.allSystems;
      arkenfox = arkenfox.lib.arkenfox;
    };
    # Supported system
    supportedSystems = builtins.attrValues {
      inherit
        (flake-utils.lib.system)
        x86_64-linux
        aarch64-linux
        ;
    };

    # After this point there is no configuration, only plumbing
    inherit (builtins) attrNames attrValues;
    inherit (nixos) lib;
    utils = import ./utils.nix {inherit lib;};
    inherit (utils) pathsToImportedAttrs readVisible importProfiles;
    eachSupportedSystem = f:
      builtins.listToAttrs
      (map (system: lib.nameValuePair system (f system)) supportedSystems);

    profiles = {
      nixos = importProfiles ./profiles/nixos;
      hm = importProfiles ./profiles/hm;
      nixvim = importProfiles ./profiles/nixvim;
    };

    pkgImport = system: unfree: pkgs:
      import pkgs {
        inherit system;
        overlays = attrValues overlays;
        config = {allowUnfree = unfree;};
      };

    pkgs-variants = system: {
      master = pkgImport system false master;
      master-unfree = pkgImport system true master;
      unstable = pkgImport system false unstable;
      unstable-unfree = pkgImport system true unstable;
      unfree = pkgImport system true nixos;
    };

    packages = import ./packages;

    nur-modules = import nur {
      # Architecture is hardcoded since it won't be used by modules
      nurpkgs = import nixos {system = "x86_64-linux";};
    };

    libModule = {
      pkgs,
      lib,
      ...
    } @ args: {
      lib =
        utils.recImport {
          dir = ./lib;
          _import = base: import "${./lib}/${base}.nix" args;
        }
        // {
          utils = import ./utils.nix args;
        }
        // extraLib;
    };

    hosts = import ./hosts {
      inherit self lib;
      inherit overlays modules;
    };
  in {
    packages =
      eachSupportedSystem (system: let
        pkgs = import nixos {
          inherit system;
          overlays = [packages];
        };
      in {
        inherit
          (pkgs)
          reupload
          ;
      })
      // {
        x86_64-linux = {
          helzvog-sd-image = nixos-generators.nixosGenerate {
            system = "aarch64-linux";
            format = "sd-aarch64";
            modules = hosts.helzvog.modules;
          };
        };
      };

    lib = import ./utils.nix {inherit lib;};

    overlays = let
      overlayDir = ./overlays;
      fullPath = name: overlayDir + "/${name}";
      overlayPaths = map fullPath (attrNames (readVisible overlayDir));
    in
      pathsToImportedAttrs overlayPaths;

    nixosModules = let
      modulesDir = ./modules/nixos;
      fullPath = name: modulesDir + "/${name}";
      modulesPaths = map fullPath (attrNames (readVisible modulesDir));
    in
      pathsToImportedAttrs modulesPaths;

    hmModules = let
      modulesDir = ./modules/hm;
      fullPath = name: modulesDir + "/${name}";
      modulesPaths = map fullPath (attrNames (readVisible modulesDir));
    in
      pathsToImportedAttrs modulesPaths;

    nixvimModules = let
      modulesDir = ./modules/nixvim;
      fullPath = name: modulesDir + "/${name}";
      modulesPaths = map fullPath (attrNames (readVisible modulesDir));
    in
      pathsToImportedAttrs modulesPaths;

    nixosConfigurations = builtins.mapAttrs (_: config:
      lib.nixosSystem {
        inherit (config) modules;
        specialArgs = {
          hardware = nixos-hardware.nixosModules;
        };
      })
    hosts;
  };
}
