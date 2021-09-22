{ config, lib, ... }:

let
  inherit (config.pkgsets) pkgs unfree;
in {
  programs.why3 = {
    enable = false;
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
        package = unfree.why3-alt-ergo;
      };

      coq-dev = {
        type = "coq";
        package = pkgs.coq;
        # TODO absolute path to emacsclient
        editor = "emacsclient -c";
      };
    };
  };
}
