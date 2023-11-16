{...}: {
  programs.nixvim.extraConfigVim = ''
    "elpi
    autocmd BufRead,BufNewFile *.elpi set filetype=lprolog

    autocmd FileType lprolog syn match   lprologIdentifier  "\<\l[-a-zA-Z\.+*/\\^<>=`'~?@#$&!_]*\>"
    autocmd FileType lprolog syn region  lprologClause start="^\<\l[-a-zA-Z\.+*/\\^<>=`'~?@#$&!_]*\>" end=" \|:-\|\."
    autocmd FileType lprolog syn match lprologClauseSymbols ":-"
    autocmd FileType lprolog syn match lprologClauseSymbols "\."
    autocmd FileType lprolog hi def link lprologClauseSymbols Type

    autocmd FileType lprolog syn keyword elpiKeyword mode macro type pred namespace rule constraint uvar shorten
    autocmd FileType lprolog syn match elpiKeyword ":before"
    autocmd FileType lprolog syn match elpiKeyword ":after"
    autocmd FileType lprolog syn match elpiKeyword ":name"
    autocmd FileType lprolog syn match elpiMacro "@\(\w\|-\)\+"
    autocmd FileType lprolog syn match elpiSpill "{"
    autocmd FileType lprolog syn match elpiSpill "}"
    autocmd FileType lprolog syn region elpiQuotation start="{{" end="}}" contains=@elpiAntiQuotation
    autocmd FileType lprolog hi def link elpiKeyword Keyword
    autocmd FileType lprolog hi def link elpiMacro Special
    autocmd FileType lprolog hi def link elpiSpill Special
  '';
}
