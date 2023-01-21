
:- module(nvim, []).
:- use_module(korrvigs(ctx)).
:- use_module(korrvigs(actions)).

ctx:declare(nvim, path, "NVIM instance socket").

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
edit(PATH) :- ctx:get(nvim, SOCK), !, edit(SOCK, PATH).

actions:register(100, DESC, nvim:edit(PATH)) :-
  ctx:get(desktop, true),
  ctx:get(pointing, path(PATH)),
  exists_file(PATH),
  file_base_name(PATH, NAME),
  concat("Open ", NAME, DESC).
