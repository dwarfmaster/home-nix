
# My Nix home

This repos contains the NixOS and home-manager configuration for all my
computers. It is organised in the following way:

- `flake.nix`: Tie everything together. It is responsible for propagating the
  modules and overlays from the inputs to the rest of the config. As of now it
  is a big spaghetti code that shouldn't be edited often.
- `utils.nix`: Helper library functions that are needed to manipulate modules,
  ie cannot depend on value set through the module system.
- `lib`: A directory of helper functions that are made available to modules
  through `config.lib`.
- `packages`: Individual packages definition, instantiation through an overlay
  in `packages/default.nix`. Packages defined here will be exposed as package
  output of the flake.
- `overlays`: Additional overlays I may need. Overlays defined here will be
  exposed as overlay output of the flake.
- `modules`: Additional modules for every module system. The subdirectories
  define at which step of the module hierarchy they should be applied. As of now
  three types of modules are supported `nixos` ones, `hm` ones and `nixvim`
  ones. Modules defined here will be exposed as `nixosModules`, `hmModules` and
  `nixvimModules` respectively in the outputs of the flake.
- `profiles`: Profiles are modules that don't define options. Most of the
  concrete configuration is done through this. Like modules, they are split
  depending on their kind, being `nixos`, `hm` or `nixvim` as of now. Each
  profile should be defined as a `default.nix` in a directory. Those files will
  be automatically found, and exposed as options that can be enabled. For
  example, a file `hm/interface/theme/default.nix` will be exposed as an option
  in home-manager named `interface.theme.enable` that when toggled will have the
  same effect as importing the previous file.
- `hosts`: Top level configurations for computers. All home-manager
  configurations must be used from those, they are not exposed otherwise.
  Semantically this is the entry points of the configurations. They are exposed
  in the `nixosConfigurations` output of the flake.

## Miscellaneous

Some effort has been done to create a uniform feeling and theme across
applications. To do so I am actually using
[nix-colors](https://github.com/Misterio77/nix-colors), but I am planning to
move to [stylix](https://github.com/danth/stylix) when after refactoring it.

After using [Doom Emacs](https://github.com/doomemacs/doomemacs) for some years,
I am in the process of moving to neovim, configured through
[nixvim](https://github.com/pta2002/nixvim). As of now the setup is a ugly mix
of part of configuration for the two of them.

Instead of setting up the packages and editor for different languages directly
in the home-manager configuration, I'd like to expose it as devshells that I can
use in different projects. Maybe do something based on
[devenv.sh](https://devenv.sh/).

As of now there is no way to handle secrets in the configuration, and this is a
problem.

This whole repository is definitely overengineered, but it is fun to hack on.
