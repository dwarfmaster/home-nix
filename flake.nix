{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "nixpkgs/master";
    nixos.url = "nixpkgs/nixos-unstable";
    stable.url = "nixpkgs/release-25.05";
    home = {
      url = "github:nix-community/home-manager";
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
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs.nixpkgs.follows = "nixos";
    };
    django.url = "github:pnmadelaine/django-nixos/main";
    imacs = {
      url = "github:TWal/imacs";
      inputs.nixpkgs.follows = "nixos";
      inputs.django-nixos.follows = "django";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixos";
      inputs.home-manager.follows = "home";
    };
    korrvigs = {
      url = "github:dwarfmaster/korrvigs";
      inputs.nixpkgs.follows = "nixos";
    };
    wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "stable";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixos";
    };
    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixos";
    };
  };

  outputs = {
    self,
    home,
    nixos,
    master,
    stable,
    nur,
    flake-utils,
    nixos-hardware,
    nixos-generators,
    impermanence,
    simple-mailserver,
    django,
    imacs,
    stylix,
    korrvigs,
    wsl,
    nixvim,
    arkenfox,
  } @ inputs: let
    # All overlays to apply
    overlays =
      self.overlays
      // {
        nur = nur.overlays.default;
        arkenfox = arkenfox.overlays.default;
        packages = self: super:
          {
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
          mailserver = simple-mailserver.nixosModules.mailserver;
          home-manager = home.nixosModules.home-manager;
          stylix = stylix.nixosModules.stylix;
          imacs = imacs.nixosModules.imacs;
          impermanence = impermanence.nixosModule;
          korrvigs = korrvigs.nixosModule;
          wsl = wsl.nixosModules.default;
        };
      hm =
        self.hmModules
        // {
          inherit libModule;
          profiles = {...}: {imports = profiles.hm;};
          arkenfox = arkenfox.hmModules.default;
          nixvim = nixvim.homeManagerModules.nixvim;
          impermanence = impermanence.nixosModules.home-manager.impermanence;
          # korrvigs = korrvigs.hmModules.default;
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
      stable = pkgImport system false stable;
      stable-unfree = pkgImport system true stable;
      unfree = pkgImport system true nixos;
    };

    packages = import ./packages;

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
      inputs = builtins.removeAttrs inputs ["self"];
    };
  in {
    packages = eachSupportedSystem (system: let
      pkgs = import nixos {
        inherit system;
        overlays = builtins.attrValues overlays ++ [packages];
      };
    in {
      inherit
        (pkgs)
        reupload
        fvim
        ;
    });
    # // {
    #   x86_64-linux = {
    #     helzvog-sd-image = nixos-generators.nixosGenerate {
    #       system = "aarch64-linux";
    #       format = "sd-aarch64";
    #       modules = hosts.helzvog.modules;
    #     };
    #   };
    # };

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
