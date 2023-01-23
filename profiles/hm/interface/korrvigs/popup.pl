
:- module(popup, []).
:- use_module(korrvigs(xdg)).
:- use_module(library(socket)).
:- use_module(korrvigs(posix)).

temp_sock(PATH) :-
  tmp_file_stream(PATH, SIN, [ extension(sock) ]),
  close(SIN), delete_file(PATH).

server_is_ready(_).

%! with(+CMD, +ARGS, +PRED, -OUT)
%  Open a popup running cmd and args, and run PRED/3 on the input and output
%  stream of the running process, unifying OUT with the last argument of
%  PRED.
with(CMD_DESC, ARGS, PRED, RES) :-
  absolute_file_name(CMD_DESC, CMD),
  nix_constant(term, TERM),
  piper(PIPER), current_prolog_flag(system_thread_id, CPID),
  append([ "--class", "popup", "-e", PIPER, SPATH, CPID, CMD ], ARGS, NARGS),
  unix_domain_socket(SOCK),
  setup_call_cleanup(
    ( temp_sock(SPATH) ),
    (setup_call_cleanup(
      ( on_signal(usr1, _, server_is_ready) ),
      (setup_call_cleanup(
        ( process_create(TERM, NARGS, [ process(PID), stdout(null), stderr(null) ]) ),
        ( % Wait for server to be ready
          pause(),
          on_signal(usr1, _, default),

          % Connect to server
          setup_call_cleanup(
            ( tcp_connect(SOCK, SPATH),
              tcp_open_socket(SOCK, SPAIR),
              stream_pair(SPAIR, OUT, IN) ),
            ( !,
              set_stream(IN, encoding(utf8)),
              set_stream(OUT, encoding(utf8)), 
              call(PRED, IN, OUT, RES) ),
            ( close(SPAIR) ))),
        ( process_wait(PID, RET) ))),
      ( on_signal(usr1, _, default) ))),
    ( delete_file(SPATH) )),
  RET = exit(0).
