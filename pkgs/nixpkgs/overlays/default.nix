args:

let

  home-manager = import ./home-manager args;

  emacs = import ./emacs args;

in [ home-manager emacs ]

