
:- module(webcomics, []).
:- multifile archive/2.
:- multifile pages/4.
:- multifile pages_post/3.
:- multifile chapters/4.
:- multifile chapters_post/3.
:- multifile identity/2.
:- multifile first_page/2.
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(wiki)).
:- use_module(korrvigs(ctx)).
:- use_module(korrvigs(fzf)).

:- use_module(library(http/http_open), [ http_open/3 ]).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_stream)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).


%     _        _   _                 
%    / \   ___| |_(_) ___  _ __  ___ 
%   / _ \ / __| __| |/ _ \| '_ \/ __|
%  / ___ \ (__| |_| | (_) | | | \__ \
% /_/   \_\___|\__|_|\___/|_| |_|___/
%                                    
% Actions

%! wiki(-UUID)
%  Get the current wiki
ctx_wiki(UUID) :- ctx:get(wiki,UUID_STR), atom_string(UUID, UUID_STR).

% TODO
view_url(_).

% Open last rad page
actions:register(100, DESC, webcomics:view_url(URL)) :-
  ctx:get(desktop, true),
  ctx_wiki(UUID),
  wiki:attr(UUID, [ atom('webcomic'), 'last-read' ], [ _, URL ]),
  wiki:title(UUID, TITLE),
  concat("Continue ", TITLE, DESC).

%! remote_call(+CALL)
%  Call CALL on a selected webcomic
remote_call_option([ COMIC, TITLE ]) :-
  wiki:attr(UUID, atom('webcomic'), COMIC),
  wiki:title(UUID, TITLE).
remote_call(CALL) :-
  bagof(COMIC, remote_call_option(COMIC), COMICS),
  fzf:select(COMICS, COMIC_STR),
  atom_string(COMIC, COMIC_STR),
  print(COMIC),
  call(CALL, COMIC).

%! jump_to_page(+COMIC)
%  Fuzzy select a page in a comic and jump to it
flip([A,B], [B,A]).
jump_to_page(COMIC) :-
  webcomic_dom(COMIC, DOM),
  webcomic_get_pages(COMIC, DOM, PAGES),
  maplist(flip, PAGES, CHOICES),
  fzf:select(CHOICES, PAGE),
  webcomics:view_url(PAGE).

% Jump to page
actions:register(50, DESC, webcomics:jump_to_page(COMIC)) :-
  ctx:get(desktop, true),
  ctx_wiki(UUID),
  wiki:attr(UUID, atom('webcomic'), COMIC),
  wiki:title(UUID, TITLE),
  concat("Jump to page of ", TITLE, DESC).
actions:register(10, "Jump to a webcomic page", webcomics:remote_call(jump_to_page)) :-
  ctx:get(desktop, true).

%! jump_to_chapter(+COMIC)
%  Fuzzy select a chapter in a comic and jump to it
jump_to_chapter(COMIC) :-
  webcomic_dom(COMIC, DOM),
  webcomic_get_chapters(COMIC, DOM, CHAPS),
  maplist(flip, CHAPS, CHOICES),
  fzf:select(CHOICES, CHAP),
  view_url(CHAP).

% Jump to chapter
actions:register(50, DESC, webcomics:jump_to_chapter(COMIC)) :-
  ctx:get(desktop, true),
  ctx_wiki(UUID),
  wiki:attr(UUID, atom('webcomic'), COMIC),
  wiki:title(UUID, TITLE),
  concat("Jump to chapter of ", TITLE, DESC).
actions:register(10, "Jump to a webcomic chapter", webcomics:remote_call(jump_to_chapter)) :-
  ctx:get(desktop, true).

%! update_metrics(-COMIC)
%  Update the metrics for the comic COMIC
update_metrics(COMIC) :-
  wiki:attr(UUID, atom('webcomic'), COMIC),
  wiki:attr(UUID, 'last-read', LAST),
  archive(COMIC, URL),
  load_html_file(URL, DOM),
  webcomic_metrics(COMIC, DOM, LAST, [_, CHAP], LEFT, NEW, NEW_CHAPTERS),
  wiki:set_attr(UUID,
    [ 'chapter', 'new-in-chapter', 'new-pages', 'new-chapters' ],
    [ CHAP,      LEFT,             NEW,         NEW_CHAPTERS   ]).

% Update metrics from last read page
actions:register(80, DESC, webcomics:update_metrics(COMIC)) :-
  ctx:get(desktop, true),
  ctx_wiki(UUID),
  wiki:attr(UUID, [ atom('webcomic'), 'last-read' ], [ COMIC, _ ]),
  wiki:title(UUID, TITLE),
  concat("Update metrics for ", TITLE, DESC).


% Save currently open page as last-read
actions:register(100, DESC, wiki:set_attr(UUID, 'last-read', URL)) :-
  ctx:get(desktop, true),
  ctx:get(web, URL),
  identify(COMIC, URL),
  atom_string(COMIC, COMIC_STR),
  wiki:attr(UUID, atom('webcomic'), COMIC_STR),
  wiki:title(UUID, TITLE),
  concat("Bookmark for ", TITLE, DESC).

%  _   _ _   _ _ _ _   _           
% | | | | |_(_) (_) |_(_) ___  ___ 
% | | | | __| | | | __| |/ _ \/ __|
% | |_| | |_| | | | |_| |  __/\__ \
%  \___/ \__|_|_|_|\__|_|\___||___/
%                                  
% Utilities

%! file_load_html(-PATH, +DOM) is det
%  Load and parse an html file at PATH, giving its DOM
file_load_html(PATH, DOM) :-
  setup_call_cleanup(open(PATH, read, In),
                     ( dtd(html, DTD),
                       load_structure(stream(In),
                                      DOM,
                                      [ dtd(DTD),
                                        dialect(sgml),
                                        shorttag(false),
                                        max_errors(-1),
                                        syntax_errors(quiet)
                                      ])
                     ),
                     close(In)).
%! http_load_html(-URL, +DOM) is det
%  Load and parse an html file online at URL, giving its DOM
http_load_html(URL, DOM) :-
  tmp_file_stream(text, TMP, WRT),
  nix_constant(curl, CURL),
  process_create(CURL, [ URL ], [ stdout(stream(WRT)), stderr(null) ]),
  close(WRT),
  file_load_html(TMP, DOM),
  delete_file(TMP).

%! webcomic_dom(-WEBCOMIC, +DOM) is det
%  Given a webcomic id, parse the relevant page
webcomic_dom(WEBCOMIC, DOM) :-
  archive(WEBCOMIC, URL),
  http_load_html(URL, DOM).
%! webcomic_pages_pair(-WEBCOMIC, -DOM, +PAIR) is nondet
%  Get a pair of a title and an url of a page of a webcomic, found
%  by scrapping the DOM of its archive page. No guarantee is made on
%  the order
webcomic_pages_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  pages(WEBCOMIC, DOM, TITLE, URL).
%! webcomic_get_pages(-WEBCOMIC, -DOM, +PAGES) is det
%  Get all PAGES as pair of a webcomic, in the chronological order
webcomic_get_pages(WEBCOMIC, DOM, PAGES) :-
  bagof(PAIR, webcomic_pages_pair(WEBCOMIC, DOM, PAIR), TMP),
  pages_post(WEBCOMIC, TMP, PAGES).
%! webcomic_chapters_pair(-WEBCOMIC, -DOM, +PAIR) is nondet
%  Same as webcomic_pages_pair but for chapters
webcomic_chapters_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  chapters(WEBCOMIC, DOM, TITLE, URL).
%! webcomic_get_chapters(-WEBCOMIC, -DOM, +CHAPTERS) is det
%  Same as webcomic_get_pages but for chapters
webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS) :-
  bagof(PAIR, webcomic_chapters_pair(WEBCOMIC, DOM, PAIR), TMP),
  chapters_post(WEBCOMIC, TMP, CHAPTERS).

page_url([ _, URL ], URL).
page_title([ TITLE, _ ], TITLE).

%! chapters_metrics_find(-PAGES, -CHAPTERS, -FROM, -ACTUAL, +CHAP, +LEFT, +NEW, +NEW_CHAPTERS) is det
%  Given the PAGES and CHAPTERS of a webcomic, compute the current chapter CHAP,
%  the number of pages to the end of the chapter +LEFT, the number of pages left to
%  the end of the webcomic +NEW, and the number of new chapters +NEW_CHAPTERS from
%  a page FROM. ACTUAL is the default chapter, used if FROM is not in any chapters.

% FROM is the first page of a chapter
chapters_metrics_find(PAGES, CHAPTERS, FROM, _, CHAP, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  append([ CHAP ], TLC, CHAPTERS),
  page_url(PAGE, FROM), page_url(CHAP, FROM), !,
  length(TL, NEW),
  length(TLC, NEW_CHAPTERS),
  chapters_metrics_compute(TL, TLC, 0, LEFT).
% New chapter
chapters_metrics_find(PAGES, CHAPTERS, FROM, _, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  append([ CHAP ], TLC, CHAPTERS),
  page_url(PAGE, URL), page_url(CHAP, URL), !,
  chapters_metrics_find(TL, TLC, FROM, CHAP, CURRENT, LEFT, NEW, NEW_CHAPTERS).
% Find from page
chapters_metrics_find(PAGES, CHAPTERS, FROM, ACTUAL, ACTUAL, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  page_url(PAGE, FROM), !,
  length(TL, NEW),
  length(CHAPTERS, NEW_CHAPTERS),
  chapters_metrics_compute(TL, CHAPTERS, 0, LEFT).
% Pass page
chapters_metrics_find(PAGES, CHAPTERS, FROM, ACTUAL, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  append([ _ ], TL, PAGES), !,
  chapters_metrics_find(TL, CHAPTERS, FROM, ACTUAL, CURRENT, LEFT, NEW, NEW_CHAPTERS).

%! chapters_metrics_compute(-PAGES, -CHAPTERS, -N, +LEFT)
%  Given a list of PAGES and CHAPTERS, compute the number of pages
%  before the beggining of the new chapters LEFT, using N as an accumulator
%  (so should be set to 0 to get the actual value).

% Count number of pages until next chapter start
chapters_metrics_compute(PAGES, CHAPTERS, LEFT, LEFT) :-
  append([ PAGE ], _, PAGES),
  append([ CHAP ], _, CHAPTERS),
  page_url(PAGE, URL), page_url(CHAP, URL), !.
chapters_metrics_compute(PAGES, [], N, LEFT) :-
  length(PAGES, LEN),
  LEFT = N + LEN.
chapters_metrics_compute([], _, LEFT, LEFT).
chapters_metrics_compute(PAGES, CHAPTERS, N, LEFT) :-
  append([ _ ], TL, PAGES),
  NN is N+1,
  chapters_metrics_compute(TL, CHAPTERS, NN, LEFT).

%! webcomic_metrics(-WEBCOMIC, -DOM, -FROM, +CURRENT, +LEFT, +NEW, +NEW_CHAPTERS) is det
%  Given a WEBCOMIC and the DOM of its archive, compute the current chapter CURRENT,
%  the number of pages to the end of the chapter +LEFT, the number of pages left to
%  the end of the webcomic +NEW, and the number of new chapters +NEW_CHAPTERS from
%  a page FROM. If there are no chapters, CURRENT is the archive page.
webcomic_metrics(WEBCOMIC, DOM, FROM, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  webcomic_get_pages(WEBCOMIC, DOM, PAGES),
  archive(WEBCOMIC, URL),
  webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS),
  chapters_metrics_find(PAGES, CHAPTERS, FROM, [ "Archive", URL ], CURRENT, LEFT, NEW, NEW_CHAPTERS).

