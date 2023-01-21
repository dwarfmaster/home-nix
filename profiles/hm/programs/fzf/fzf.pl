
:- module(fzf, []).
:- use_module(korrvigs(popup)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(server)).

split_and_num(COUNT, [[OPT,D]|TL], [OPT|OPTS], [[COUNT, D]|DS]) :-
  NC is COUNT + 1,
  split_and_num(NC, TL, OPTS, DS).
split_and_num(_, [], [], []).

handler_split(CHOICES, IN, OUT, RES) :-
  split_and_num(0, CHOICES, RESULTS, TO_CHOOSE),
  forall(member([OPT, DESC], TO_CHOOSE), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_string(OUT, " ", "", _, ID_STR), !,
  number_string(ID, ID_STR),
  nth0(ID, RESULTS, RES).

handler(CHOICES, IN, OUT, RES) :-
  forall(member([OPT, DESC], CHOICES), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_string(OUT, " ", "", _, RES).

%! run(+CHOICES, +OPTS, -R)
%! run_any(+CHOICES, +OPTS, -R)
%  Given a list CHOICES of pair of values and description, open a fuzzy finder
%  on the descriptions and return the selected associated value. Additional
%  options can be given to fzf with OPTS. Values may not include spaces. In run_any,
%  values may include spaces (and can be any prolog term), but the previewer won't
%  have access to them.
run(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler(CHOICES), R).
run_any(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler_split(CHOICES), R).

%! select(+CHOICES, -CHOICE)
%  Given a list of choices, run fzf to get a choice
select(CHOICES, CHOICE) :-
  run_any(CHOICES, [], CHOICE).

%! select_text(+CHOICES, -CHOICE)
%  Same as select, but assume the values are path to text files, enabling
%  a previewer in fzf
select_text(CHOICES, CHOICE) :-
  run(CHOICES, [ "--preview", "bat --color=always {+1}" ], CHOICE).

%! select_action
%  Use fzf to select an action to run
select_action :-
  actions:list(ACTS),
  maplist(reverse, ACTS, OPTS),
  select(OPTS, CODE),
  call(CODE).

%! register(+PRIORITY, +DESC, +CODE)
%  Each success is an action with a priority, a description and a code
%  to execute. It should fail if the action is not meant to be applied.
actions:register(0, "Select an action to run", fzf:select_action) :- ctx:get(desktop, true).
:- server:register("select", fzf:select_action).
