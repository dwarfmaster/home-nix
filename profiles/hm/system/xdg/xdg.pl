
:- module(xdg, [ ]).

%! dir_get(-ENV, +PATH)
%  Creates $ENV/korrvigs if it doesn't exists and unify PATH with it
dir_get(ENV, PATH) :-
  getenv(ENV, DIR),
  directory_file_path(DIR, "korrvigs", PATH),
  (exists_directory(PATH) -> true; make_directory(PATH)).

runtime(PATH) :- dir_get("XDG_RUNTIME_DIR", PATH).
config(PATH) :- nix_constant('xdg-config', PATH).
cache(PATH) :- nix_constant('xdg-cache', PATH).
data(PATH) :- nix_constant('xdg-data', PATH).
state(PATH) :- nix_constant('xdg-state', PATH).
