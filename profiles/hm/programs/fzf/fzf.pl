
:- module(fzf, []).
:- use_module(korrvigs(popup)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(server)).

split_and_num(COUNT, [[OPT,D]|TL], [OPT|OPTS], [[COUNT, D]|DS]) :-
  NC is COUNT + 1,
  split_and_num(NC, TL, OPTS, DS).
split_and_num(_, [], [], []).

%! read_all(+STREAM, -RES)
%  Read all line from STREAM, and take the first word from each, returning
%  the list of all those words. Has a specific case for the first line, as
%  it is the printed query.
read_all(STREAM, []) :-
  at_end_of_stream(STREAM).
read_all(STREAM, [WD|TL]) :-
  read_string(STREAM, "\n", "", _, WD),
  read_all_rec(STREAM, TL).
read_all_rec(STREAM, []) :-
  at_end_of_stream(STREAM).
read_all_rec(STREAM, [WD|TL]) :-
  read_string(STREAM, " ", "", _, WD),
  read_string(STREAM, "\n", "", _, _),
  read_all_rec(STREAM, TL).

handler_split(NEW, CHOICES, IN, OUT, RES) :-
  split_and_num(0, CHOICES, RESULTS, TO_CHOOSE),
  forall(member([OPT, DESC], TO_CHOOSE), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_all(OUT, IDS_STR), !,
  process_result(NEW, IDS_STR, PROCESSED),
  ( PROCESSED = new(_) -> RES = PROCESSED
  ; maplist(
      {RESULTS}/[ID_STR,ID_RES]>> (number_string(ID, ID_STR), nth0(ID, RESULTS, ID_RES)),
      PROCESSED,
      RES) ).

handler(NEW, CHOICES, IN, OUT, RES) :-
  forall(member([OPT, DESC], CHOICES), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_all(OUT, RQ),
  process_result(NEW, RQ, RES).

process_result(true, [QUERY], new(QUERY)) :- !.
process_result(false, [_], []) :- !.
process_result(_, [_|R], R).
process_result(_, [], []).

%! run(+CHOICES, +OPTS, +NEW, -R)
%! run_any(+CHOICES, +OPTS, +NEW, -R)
%  Given a list CHOICES of pair of values and description, open a fuzzy finder
%  on the descriptions and return the selected associated value. Additional
%  options can be given to fzf with OPTS. Values may not include spaces. In
%  run_any, values may include spaces (and can be any prolog term), but the
%  previewer won't have access to them. R is a list of all selected items (or
%  just a singleton if fzf is not launched in multi mode). If NEW=true, can
%  succeed on a new query entered by the user, which will be given wrapped on
%  the new(...) functor instead of a list.
run(CHOICES, OPTS, NEW, R) :-
  append([ "--read0", "--with-nth=2..", "--print-query" ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler(NEW, CHOICES), R), !.
run_any(CHOICES, OPTS, NEW, R) :-
  append([ "--read0", "--with-nth=2..", "--print-query" ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler_split(NEW, CHOICES), R), !.

%! preview_cmd(-PREV, +CMD)
%  Given the value of the preview option to select, give the command to run.
:- multifile preview_cmd/2.
preview_cmd(text, "bat --color=always --line-range=:500 {1}").
preview_cmd(dir, "lsd --color=always --tree -d {1}").
preview_cmd(cmd(CMD), CMD).

opts_to_args([], [], true, false, false).
opts_to_args([preview(P)|OPTS], ["--preview" | [ CMD | ARGS ] ], false, MULTI, NEW) :- !,
  preview_cmd(P, CMD),
  opts_to_args(OPTS, ARGS, _, MULTI, NEW).
opts_to_args([multi(true)|OPTS], NARGS, ANY, true, NEW) :- !,
  append(["--multi", "--marker="], ARGS, NARGS),
  opts_to_args(OPTS, ARGS, ANY, _, NEW).
opts_to_args([multi(false)|OPTS], ARGS, ANY, MULTI, NEW) :- !,
  opts_to_args(OPTS, ARGS, ANY, MULTI, NEW).
opts_to_args([extra(EXTRA)|OPTS], NARGS, ANY, MULTI, NEW) :- !,
  append(EXTRA, ARGS, NARGS),
  opts_to_args(OPTS, ARGS, ANY, MULTI, NEW).
opts_to_args([new(true)|OPTS], NARGS, ANY, MULTI, true) :- !,
  append(["--bind", "ctrl-j:print-query"], ARGS, NARGS),
  opts_to_args(OPTS, ARGS, ANY, MULTI, _).
opts_to_args([new(false)|OPTS], ARGS, ANY, MULTI, NEW) :- !,
  opts_to_args(OPTS, ARGS, ANY, MULTI, NEW).
opts_to_args([OPT|_], _, _, _, _) :-
  throw(fzf_unknown_option(OPT)).

%! select(+CHOICES, -CHOICE, +OPTS)
%  Given a list of choices, run fzf to get a choice, but assume the values are
%  path to text files, enabling a previewer in fzf. Paths must not have spaces
%  in them. OPTS is a list of options. Supported options are:
%  - preview(P): setup a previewer for fzf. When using a previewer, the values
%    must not contain spaces. The supported values for P are:
%    - text: assumes values are path to text files, print them with colors
%    - dir: assumes values are path to directories, print their tree
%    - cmd(CMD): CMD must be a string that will be passed as the preview option
%      to fzf
%    - can be extended using the preview_cmd predicate
%  - multi(B): Enable multiple selection if B=true (if not set it is assumed
%    to be false), in which case CHOICE will be the list a selected values,
%    instead of a single ones.
%  - new(B): Enable user to enter a new value (and use ctrl+j to select it).
%  - extra(OPTS): A list of extra options to give to fzf.
%  CHOICE will be:
%  - A list of selected values if multi is true
%  - The single selected value if multi is false
%  - The query wrapped in the functor new(...) if new is true and the user
%    entered a new string (otherwise the result follows the above rules).
select(CHOICES, CHOICE, OPTS) :-
  opts_to_args(OPTS, ARGS, ANY, MULTI, NEW),
  (ANY = true -> run_any(CHOICES, ARGS, NEW, RES) ; run(CHOICES, ARGS, NEW, RES)),
  (MULTI = true -> CHOICE = RES ;
    (is_list(RES) -> [ CHOICE ] = RES ; CHOICE = RES)).

%! select(+CHOICES, -CHOICE)
%  Same as select/3 but with empty options list.
select(CHOICES, CHOICE) :- select(CHOICES, CHOICE, []).

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
actions:register(0, "Select an action to run", fzf:select_action) :- ctx:desktop.
:- server:register("select", fzf:select_action).
