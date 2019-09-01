{ pkgs, ... }:

{
  # Made it works thanks to https://github.com/gloaming/nixpkgs/tree/feature/nightly-neovim
  neovim-nightly = 
    let
      pkgs = import <nixos-unstable> {};

      overrides = self: super: {
        luv-dev = super.luv.override ({
          propagatedBuildInputs = [
            pkgs.libuv
          ];
          preBuild = ''
           sed -i 's,\(option(WITH_SHARED_LIBUV.*\)OFF,\1ON,' CMakeLists.txt
           sed -i 's,\(option(BUILD_MODULE.*\)ON,\1OFF,' CMakeLists.txt
           sed -i 's,$''+''{INSTALL_INC_DIR},${placeholder "out"}/include/luv,' CMakeLists.txt
           rm -rf deps/libuv
          '';
          postInstall = ''
            rm -rf $out/luv-*-rocks
          '';
        });

        lua = super.lua // { pkgs = self; };
      };
      lua = (pkgs.luajitPackages.override { inherit overrides; }).lua;

      neovimLuaEnv = pkgs.lua.withPackages(ps:
        ( with ps; [ mpack lpeg luv luabitop compat53 ] ));
    in with pkgs; stdenv.mkDerivation rec {
    name = "neovim-nightly-${version}";
    version = "e29b89ca54e20b1f3430b43b1c57bd72ccadf954";

    # Taken from neovim-unwrapped derivation
    meta = {
      description = "Vim text editor fork focused on extensibility and agility";
      longDescription = ''
        Neovim is a project that seeks to aggressively refactor Vim in order to:
        - Simplify maintenance and encourage contributions
        - Split the work between multiple developers
        - Enable the implementation of new/modern user interfaces without any
          modifications to the core source
        - Improve extensibility with a new plugin architecture
      '';
      homepage    = https://www.neovim.io;
      # "Contributions committed before b17d96 by authors who did not sign the
      # Contributor License Agreement (CLA) remain under the Vim license.
      # Contributions committed after b17d96 are licensed under Apache 2.0 unless
      # those contributions were copied from Vim (identified in the commit logs
      # by the vim-patch token). See LICENSE for details."
      # Both licenses and maintainers don't work with unstable
      # license = with licenses; [ asl20 vim ];
      # maintainers = with maintainers; [ lucas8 ];
      # `lua: bad light userdata pointer`
      # https://nix-cache.s3.amazonaws.com/log/9ahcb52905d9d417zsskjpc331iailpq-neovim-unwrapped-0.2.2.drv
      broken = stdenv.isAarch64;
    };

    src = fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "${version}";
      sha256 = "1mp6bc0wl25pckgqx75wgcqlb601dh6385brwl9199r3xb308rhc";
    };

    patches = [
      ./system_rplugin_manifest.patch
    ];

    enableParallelBuilding = true;
    dontFixCmake = true;

    buildInputs = [
      libtermkey
      libuv
      msgpack
      ncurses
      libvterm-neovim
      unibilium
      gperf
      neovimLuaEnv
      jemalloc
      lua.pkgs.luv
    ];

    # NeoVim will keep dependencies to build tools because it references them
    # in its --version. We won't bother patching that
    nativeBuildInputs = [
      cmake
      gettext
      pkgconfig
    ];

    cmakeFlags = [
      "-DLUA_PRG=${neovimLuaEnv}/bin/lua"
      "-DGPERF_PRG=${gperf}/bin/gperf"
      "-DLIBLUV_LIBRARY=${lua.pkgs.luv-dev}/lib/lua/${lua.luaversion}/libluv.a"
      "-DLIBLUV_INCLUDE_DIR=${lua.pkgs.luv-dev}/include"
    ];

    hardeningDisable = [ "fortify" ];

    postInstall = stdenv.lib.optionalString stdenv.isLinux ''
      sed -i -e "s|'xsel|'${xsel}/bin/xsel|g" $out/share/nvim/runtime/autoload/provider/clipboard.vim
    '';
  };
}

