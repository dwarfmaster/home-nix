args:

let
  home-manager = import ./home-manager args;
  emacs = import ./emacs args;
  why3 = import ./why3 args;
in {
  v19-09 = [ home-manager.v19-09 ];
  v20-03 = [ home-manager.v20-03 emacs.v20-03 ];
  v20-09 = [ home-manager.v20-09 emacs.v20-09 why3.v20-09 ];
  unstable = [ home-manager.unstable emacs.unstable why3.unstable ];
}

