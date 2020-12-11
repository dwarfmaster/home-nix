general:

let
  pkgs = general.pkgs.main;
  buildPlugin = (pkgs.vimUtils.override { vim = pkgs.vim; }).buildVimPluginFrom2Nix;
in with pkgs; {
  base16-vim-lightline = buildPlugin {
    pname = "base16-vim-lightline";
    version = "2019-06-24";
    src = fetchFromGitHub {
      owner  = "mike-hearn";
      repo   = "base16-vim-lightline";
      rev    = "cea11b7d0a407eccf0085feefb5e3be2ec99bdbb";
      sha256 = "0lcxcikkbk03lvsjjjqv22ypl9wpa7zkb6ndbc2gasskckzk3fa9";
    };
  };

  # Necessary while waiting for nixpkgs to update its version of base16-vim
  # Indeed, the nixpkgs version is broken with neovim nightly
  base16-vim-recent = buildPlugin {
    pname = "base16-vim-recent";
    version = "2019-06-07";
    src = fetchFromGitHub {
      owner  = "chriskempson";
      repo   = "base16-vim";
      rev    = "6191622d5806d4448fa2285047936bdcee57a098";
      sha256 = "1qz21jizcy533mqk9wff1wqchhixkcfkysqcqs0x35wwpbri6nz8";
    };
  };

  coquille = buildPlugin rec {
    pname = "coquille";
    version = "81cea3080eb781216c4f3ab14dc7a1673d6688f6";
    src = fetchFromGitLab {
      domain = "framagit.org";
      owner  = "tyreunom";
      repo   = "coquille";
      rev    = "${version}";
      sha256 = "0r0nx5jnjkfx3n1y3lplvsn35iw7gqsbgm7r150559sdbag1ahw0";
    };
  };
}
