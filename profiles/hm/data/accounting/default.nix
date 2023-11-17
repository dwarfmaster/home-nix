{
  config,
  lib,
  pkgs,
  ...
}: let
  cmp-hledger = pkgs.vimUtils.buildVimPlugin {
    name = "cmp-hledger";
    src = pkgs.fetchFromGitHub {
      owner = "kirasok";
      repo = "cmp-hledger";
      rev = "1d237ed9f5b8748348d600741ef15653050023fb";
      sha256 = "1nim0bnryl2684aqwfpj1b5gaa5080lab3s02rfh3wr95aq8zzp4";
    };
  };
in {
  home.packages = with pkgs; [
    haskellPackages.hledger
  ];

  programs.nixvim = {
    extraPlugins = [ cmp-hledger ];
    plugins.nvim-cmp.sources = [
      {name = "hledger";}
    ];
  };
}
