
:- module(bspwm, []).
:- use_module(korrvigs(ctx)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(fzf)).
:- use_module(library(http/json)).
:- use_module(library(dcg/basics)).


%  _   _ _   _ _ _ _   _           
% | | | | |_(_) (_) |_(_) ___  ___ 
% | | | | __| | | | __| |/ _ \/ __|
% | |_| | |_| | | | |_| |  __/\__ \
%  \___/ \__|_|_|_|\__|_|\___||___/
%                                  
% Utilities

%! get_monitors(-MONS)
%  Return list of monitors available to bspwn
get_monitors(MONS) :-
  nix_constant(bspc, BSPC),
  setup_call_cleanup(
    process_create(BSPC, [ "query", "-M", "--names" ], [ stdout(pipe(OUT)) ]),
    bagof(MON, read_line_to_string(OUT, MON), MONS),
    close(OUT)).

%! get_focused_monitor(-MON)
%  Return the name of the focused monitor
get_focused_monitor(MON) :-
  nix_constant(bspc, BSPC),
  setup_call_cleanup(
    process_create(BSPC, [ "query", "-M", "-m", "--names" ], [ stdout(pipe(OUT)) ]),
    read_line_to_string(OUT, MON),
    close(OUT)).

%! get_focused_desktop(-WORKSPACE, -DESK)
%  Return the workspace and name of the focused desktop
get_focused_desktop(WORK, DESK) :-
  nix_constant(bspc, BSPC),
  setup_call_cleanup(
  process_create(BSPC, [ "query", "-D", "-d", "--names" ], [ stdout(pipe(OUT)) ]),
    ( read_line_to_string(OUT, BDESK), split_desktop(BDESK, WORK, DESK) ),
    close(OUT)).

%! get_state(-MON, -STATE)
%  Execute bspc query to get the full state of a monitor of bspwm as
%  a json object, along with the monitor name. Succeeds once for each
%  monitor.
get_state(MON, STATE) :-
  get_monitors(MONS), member(MON, MONS),
  nix_constant(bspc, BSPC),
  setup_call_cleanup(
    process_create(BSPC, [ "query", "-T", "-m", MON ], [ stdout(pipe(OUT)) ]),
    json_read(OUT, STATE),
    close(OUT)).

%! find_client_leaves(+FID, +NODE, -ID, -CLASS, -INSTANCE, -FOCUS)
%  Given a NODE is the bspwm tree of window, recurse to find each window,
%  giving its ID, class, instance and focus. Focus is found by comparing the
%  window id with FID.
find_client_leaves(FID, NODE, ID, CLASS, INSTANCE, FOCUS) :-
  member(firstChild=json(CHILD), NODE),
  find_client_leaves(FID, CHILD, ID, CLASS, INSTANCE, FOCUS).
find_client_leaves(FID, NODE, ID, CLASS, INSTANCE, FOCUS) :-
  member(secondChild=json(CHILD), NODE),
  find_client_leaves(FID, CHILD, ID, CLASS, INSTANCE, FOCUS).
find_client_leaves(FID, NODE, ID, CLASS, INSTANCE, FOCUS) :-
  member(client=json(CLIENT), NODE),
  member(id=ID, NODE),
  member(className=CLASS, CLIENT),
  member(instanceName=INSTANCE, CLIENT),
  (FID = ID -> FOCUS = 1; FOCUS = 0).

%! split_desktop(+BSP_DESK, -WORKSPACE, -DESK)
%  Given the bspwm name of the desktop, extract the name of the
%  workspace and desktop.
split_desktop(IN, W, r4) :- concat(W, "^r4", IN).
split_desktop(IN, W, r3) :- concat(W, "^r3", IN).
split_desktop(IN, W, r2) :- concat(W, "^r2", IN).
split_desktop(IN, W, r1) :- concat(W, "^r1", IN).
split_desktop(IN, W, r0) :- concat(W, "^r0", IN).
split_desktop(IN, W, l0) :- concat(W, "^l0", IN).
split_desktop(IN, W, l1) :- concat(W, "^l1", IN).
split_desktop(IN, W, l2) :- concat(W, "^l2", IN).
split_desktop(IN, W, l3) :- concat(W, "^l3", IN).
split_desktop(IN, W, l4) :- concat(W, "^l4", IN).


%! x11_id_cvt(+DEC, -HEX)
%  Convert between decimal and hexadecimal representation of x11 ID
x11_id_cvt(DEC, HEX) :- format(string(HEX), "0x~16r", DEC).


%! clients(-MON, -WORK, -DESK, -ID, -CLASS, -INSTANCE, -FOCUS)
%  Succeeds once for each client, giving the name of the monitor it is on,
%  the name of the workspace, the name of the desktop, the ID of the windows,
%  its class, its instance and whether it is the focused window or not.
clients(MON, WORKSPACE, DESKTOP, ID, CLASS, INSTANCE, FOCUS) :-
  get_focused_monitor(FMON),
  get_state(MON, json(STATE)),
  member(focusedDesktopId=FDESK, STATE),
  member(desktops=DESKS, STATE), member(json(DESK), DESKS),
  member(name=DNAME, DESK), split_desktop(DNAME, WORKSPACE, DESKTOP),
  member(id=DID, DESK),
  member(focusedNodeId=FNODE, DESK),
  member(root=json(ROOT), DESK),
  ( (FMON = MON, FDESK = DID) -> FOCUSED = FNODE; FOCUSED = 0 ),
  find_client_leaves(FOCUSED, ROOT, CID, CLASS, INSTANCE, FOCUS),
  x11_id_cvt(CID, ID).


%   ____            _            _   
%  / ___|___  _ __ | |_ _____  _| |_ 
% | |   / _ \| '_ \| __/ _ \ \/ / __|
% | |__| (_) | | | | ||  __/>  <| |_ 
%  \____\___/|_| |_|\__\___/_/\_\\__|
%                                    
% Context

%! focused_monitor(MON)
%  Get the currently focused monitor
focused_monitor(MON) :-
  ctx:desktop,
  ctx:memoise(bspwm_focused_monitor, get_focused_monitor, MON).

%! focused_window(MON, WORK, DESK, ID, CLASS, INSTANCE)
%  Get the informations about the currently focused window
focused_window(MON, WORK, DESK, ID, CLASS, INSTANCE) :-
  ctx:desktop,
  clients(MON, WORK, DESK, ID, CLASS, INSTANCE, 1).



%     _        _   _                 
%    / \   ___| |_(_) ___  _ __  ___ 
%   / _ \ / __| __| |/ _ \| '_ \/ __|
%  / ___ \ (__| |_| | (_) | | | \__ \
% /_/   \_\___|\__|_|\___/|_| |_|___/
%                                    
% Actions

%! xprop_line(PROP, TYPE, VALUE)
%  DCG for simple xprop line with string value
xprop_line(PROP, TYPE, VALUE) -->
  string_without("(", PROP_CDS), "(", { string_codes(PROP, PROP_CDS) },
  string_without(")", TYPE_CDS), ")", { string_codes(TYPE, TYPE_CDS) },
  blanks, "=", blanks,
  "\"", string_without("\"", VALUE_CDS), "\"", { string_codes(VALUE, VALUE_CDS) }.

%! pretty_window(-WIN, +PRETTY)
%  Given a pair of a window ID and its class, returns a pair of the same id
%  and a pretty string presenting the window (including the name of the
%  window).
pretty_window([ID, CLASS], [ID, PRETTY]) :-
  nix_constant(xprop, XPROP),
  setup_call_cleanup(
    process_create(XPROP, [ "-id", ID, "_NET_WM_NAME" ], [ stdout(pipe(OUT)) ]),
    read_line_to_codes(OUT, PROP),
    close(OUT)),
  phrase(xprop_line(_, _, NAME), PROP, []),
  format(string(PRETTY), "[~s] ~s", [ CLASS, NAME ]).

%! focus_window
%  Create a list of all windows not in buffer but in current workspace, select
%  one using fzf and move the focus to it.
focus_window :-
  get_focused_desktop(WORK, _),
  bagof([ID,CLASS], M^D^I^clients(M, WORK, D, ID, CLASS, I, 0), CLIENTS),
  maplist(pretty_window, CLIENTS, PRETTIES),
  fzf:select(PRETTIES, C), !,
  nix_constant(bspc, BSPC),
  process_create(BSPC, [ "node", "-f", C ], []).
actions:register(40, DESC, bspwm:focus_window) :-
  ctx:desktop,
  get_focused_desktop(WORK, _),
  format(string(DESC), "Focus window in ~s", WORK).
:- server:register("focus_window", bspwm:focus_window).

% TODO manage buffer
