
:- module(nvim, []).
:- use_module(korrvigs(ctx)).
:- use_module(korrvigs(actions)).

%! instance(SOCK)
%  Path to the socket of the currently running nvim instance
instance(_) :- false.

%! view(+SOCK, +FILE)
%  Open a file in neovim as readonly file
view(SOCK, PATH) :-
  nix_constant(nvim, NVIM),
  process_create(NVIM, [ "--server", SOCK, "--remote", "-R", PATH ], [ ]).

%! edit(+SOCK, +FILE)
%  Open a file in neovim
edit(SOCK, PATH) :-
  nix_constant(nvim, NVIM),
  process_create(NVIM, [ "--server", SOCK, "--remote", PATH ], [ ]).

%! edit(+FILE)
%  Open file in current neovim
edit(PATH) :- instance(SOCK), !, edit(SOCK, PATH).

actions:register(100, DESC, nvim:edit(SOCK, PATH)) :-
  ctx:desktop,
  ctx:pointing(file(PATH)),
  instance(SOCK),
  exists_file(PATH),
  file_base_name(PATH, NAME),
  concat("Open ", NAME, DESC).
