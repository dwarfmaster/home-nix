
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
%  the list of all those words.
read_all(STREAM, []) :-
  at_end_of_stream(STREAM).
read_all(STREAM, [WD|TL]) :-
  read_string(STREAM, " ", "", _, WD),
  read_string(STREAM, "\n", "", _, _),
  read_all(STREAM, TL).

handler_split(CHOICES, IN, OUT, RES) :-
  split_and_num(0, CHOICES, RESULTS, TO_CHOOSE),
  forall(member([OPT, DESC], TO_CHOOSE), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_all(OUT, IDS_STR), !,
  maplist(
    {RESULTS}/[ID_STR,ID_RES]>> (number_string(ID, ID_STR), nth0(ID, RESULTS, ID_RES)),
    IDS_STR,
    RES).

handler(CHOICES, IN, OUT, RES) :-
  forall(member([OPT, DESC], CHOICES), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_all(OUT, RES).

%! run(+CHOICES, +OPTS, -R)
%! run_any(+CHOICES, +OPTS, -R)
%  Given a list CHOICES of pair of values and description, open a fuzzy finder
%  on the descriptions and return the selected associated value. Additional
%  options can be given to fzf with OPTS. Values may not include spaces. In run_any,
%  values may include spaces (and can be any prolog term), but the previewer won't
%  have access to them. R is a list of all selected items (or just a singleton if fzf
%  is not launched in multi mode).
run(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler(CHOICES), R), !.
run_any(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  nix_constant(fzf, FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler_split(CHOICES), R), !.

preview_cmd(text, "bat --color=always --line-range=:500 {1}").
preview_cmd(dir, "lsd --color=always --tree -d {1}").
preview_cmd(cmd(CMD), CMD).

opts_to_args([], [], true, false).
opts_to_args([preview(P)|OPTS], ["--preview" | [ CMD | ARGS ] ], false, MULTI) :- !,
  preview_cmd(P, CMD),
  opts_to_args(OPTS, ARGS, _, MULTI).
opts_to_args([multi(true)|OPTS], NARGS, ANY, true) :- !,
  append(["--multi", "--marker="], ARGS, NARGS),
  opts_to_args(OPTS, ARGS, ANY, _).
opts_to_args([multi(false)|OPTS], ARGS, ANY, MULTI) :- !,
  opts_to_args(OPTS, ARGS, ANY, MULTI).
opts_to_args([extra(EXTRA)|OPTS], NARGS, ANY, MULTI) :- !,
  append(EXTRA, ARGS, NARGS),
  opts_to_args(OPTS, ARGS, ANY, MULTI).
opts_to_args([OPT|_], _, _, _) :-
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
%  - multi(B): Enable multiple selection if B=true (if not set it is assumed
%    to be false), in which case CHOICE will be the list a selected values,
%    instead of a single ones.
%  - extra(OPTS): A list of extra options to give to fzf.
select(CHOICES, CHOICE, OPTS) :-
  opts_to_args(OPTS, ARGS, ANY, MULTI),
  (ANY = true -> run_any(CHOICES, ARGS, RES) ; run(CHOICES, ARGS, RES)),
  (MULTI = true -> CHOICE = RES ; [ CHOICE ] = RES).

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
actions:register(0, "Select an action to run", fzf:select_action) :- ctx:get(desktop, true).
:- server:register("select", fzf:select_action).
