{ config, lib, pkgs, ... }:

{
  imports = [ ../../../profiles/why3 ];

  # Coq, Idris and Agda
  home.packages = with pkgs; [
    coq
    # (idris.withPackages
    #   (with idrisPackages; [ lightyear contrib ]))
    agda
  ];

  # Why3
  programs.why3 = {
    enable = true;
    darkTheme = true;
    fontSize = 14;

    provers = {
      z3-dev = {
        type = "z3";
        package = pkgs.why3-z3;
      };

      cvc4-dev = {
        type = "cvc4";
        package = pkgs.why3-cvc4;
      };

      alt-ergo-dev = {
        type = "alt-ergo";
        package = pkgs.why3-alt-ergo;
      };

      coq-dev = {
        type = "coq";
        package = pkgs.coq;
        editor = "emacs";
      };
    };
  };
}
