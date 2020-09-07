args:

let

  doom-pkg = import ./master;

in

  args.pkgs.callPackage doom-pkg

