{ pkgs, ... }:

let buildPlugin = (pkgs.vimUtils.override { vim = pkgs.vim; }).buildVimPluginFrom2Nix; in

with pkgs; {
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

  sandwich = buildPlugin {
    pname = "vim-sandwich";
    version = "2019-06-11";
    src = fetchFromGitHub {
      owner  = "machakann";
      repo   = "vim-sandwich";
      rev    = "35948af32203282ffd724f4ffac87af9c3efcf11";
      sha256 = "0rpwkwi6xjd26rqvi4spbj87rp056m8r1pyncjfwmd3662xpxvgg";
    };
  };

  coc = buildPlugin rec {
    pname = "coc-nvim";
    version = "0.0.74";
    src = fetchFromGitHub {
      owner  = "neoclide";
      repo   = "coc.nvim";
      rev    = "v${version}";
      sha256 = "1s4nib2mnhagd0ymx254vf7l1iijwrh2xdqn3bdm4f1jnip81r10";
    };
  };
}

