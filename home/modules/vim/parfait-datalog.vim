" ViM Syntax File
" Language: Oracle Parfait Datalog
" Maintainer: DwarfMaster <luc@dwarfmaster.net>
" Latest Revision: February 12, 2020

if exists("b:current_syntax")
  finish
endif

" Standard datalog
syn match  dlPredDecl  '^\(\a\|_\)\+\s*(\_[^)]*)\s*$' contains=dlPredTyped keepend
syn match  dlPredRule  '^\(\a\|_\)\+\s*(\_[^)]*)\s*:-\_.\{-}\.\s*$' contains=dlFullPred keepend
syn match  dlAxiom     '^\(\a\|_\)\+\s*(\_[^)]*)\.\s*$' contains=dlFullPred keepend

syn match  dlPredTyped contained '^\(\a\|_\)\+\s*(\_[^)]*)' contains=dlPred,dlArgsTyped keepend
syn match  dlFullPred  contained '\(\a\|_\)\+\s*(\_[^)]*)' contains=dlPred,dlArgs keepend

syn match  dlPred      contained '\(\a\|_\)\+'
syn region dlArgsTyped contained start='(' end=')' contains=dlVar,dlCst,dlTpCtx
syn region dlArgs      contained start='(' end=')' contains=dlVar,dlCst,dlString
syn match  dlVar       contained '\a\(\d\|\a\)*'
syn match  dlTpCtx     contained ':\_s*\a\(\d\|\a\)\+\_s*\(,\|)\)' contains=dlType
syn match  dlType      contained '\a\(\d\|\a\)*'
syn match  dlCst       contained '\d\+'
syn match  dlCst       contained '_'
syn region dlString    contained start='"' skip='\\.' end='"'

" c++ strings
syn include @CPP syntax/cpp.vim
syn region dlCppStringA contained matchgroup=dlCppDelimiter start='"' skip='\\.' end='"' contains=@CPP,dlCppExt keepend
syn region dlCppStringB contained matchgroup=dlCppDelimiter start='{{' end='}}' contains=@CPP,dlCppExt keepend
syn region dlCppSmallString contained start='"' skip='\\.' end='"'
syn match dlCppExt contained '\$\a\(\a\|\d\|_\)*'
call matchadd('dlCppExt', '\$\a\(\a\|\d\|_\)*')

" Input rules
syn match dlInput       contained 'input\_.\{-}\.\s*$' contains=dlInputCost,dlInputK,dlInputAction,dlInputReq,dlFilterK,dlWriter,dlSet,dlCppStringA,dlCppStringB keepend
syn match dlInputK      contained 'input'
syn match dlInputCost   contained 'cost\s*\d\+' contains=dlCst
syn match dlInputAction contained '\(^\|input\)\s*\(key\|index\|semiindex\|output\)\s*\((\([^")]\|"[^"]*"\)*)\)\=' contains=dlInputK,dlInputAK,dlInputAArgs keepend
syn keyword dlInputAK   contained key index semiindex output
syn match dlInputAArgs  contained '(\([^")]\|"[^"]*"\)*)' contains=dlVar,dlCppStringA
syn match dlInputReq    contained '\(^\|input\)\s*requires\s*\(\a\|_\)*' contains=dlInputK,dlVar,dlReqK
syn keyword dlReqK      contained requires
syn match dlFilterK     contained '^\s*filter'
syn match dlWriter      contained '\(^\|input\)\s*writer\s*\(\a\|_\)*' contains=dlInputK,dlVar,dlWriterK
syn keyword dlWriterK   contained writer
syn match dlSet         contained 'set\s\+\(\a\|_\)\+=' contains=dlSetK,dlVar
syn keyword dlSetK      contained set

" Input clauses
syn match dlInputClause '^\(\a\|_\)\+\s*(\_[^)]*)\s*input\_.\{-}\.\s*$' contains=dlInput,dlPredTyped keepend

" Type clauses
syn match dlTypeClause '^.type \(\a\|_\)\+\s*"\(\\"\|[^"]\)*"\(\s*:\s*\(\(\a\|_\)\+\s*,\s*\)\=\(\a\|_\)\+\)\=\s*input\_.\{-}\.\s*$' contains=dlInput,dlTypeK,dlCppSmallString,dlTypeColon keepend
syn match dlTypeK contained '^.type'
syn match dlTypeColon contained ':'
syn match dlTypeColon contained ','

" Dependency clause
syn region dlDependency matchgroup=dlDepK start='^.dependency' end='$' contains=dlCppSmallString

" Global clause
syn region dlGlobal start='^.global' end='=.*$' contains=dlGlobalType,dlGlobalK,dlGlobalVal keepend
syn match dlGlobalK    contained '^.global'
syn match dlGlobalEq   contained '='
syn match dlGlobalS    contained ':'
syn match dlGlobalType contained ':\_s*\(\a\|_\)\+' contains=dlGlobalS
syn match dlGlobalVal  contained '=.*$' contains=dlGlobalEq

" Goal clause
syn region dlGoal matchgroup=dlGoalK start='^.goal' end='$' keepend contains=dlFullPred

" Macros
syn region dlMacro start='^#' skip='\\$' end='$' contains=dlMacroInclude,dlComment keepend
syn match dlMacroInclude contained 'include\s\+<[^>]*>' contains=dlMacroIncluded
syn match dlMacroIncluded contained '<[^>]*>'

" Comments
syn keyword dlTodo contained DEPRECATED TODO FIXME
syn match dlComment '//.*$' contains=dlTodo,@Spell
syn region dlComment start='/\*' end='\*/' contains=dlTodo,@Spell

" Colors
hi def link dlComment        Comment
hi def link dlTodo           Todo
hi def link dlBody           Delimiter
hi def link dlPred           Function
hi def link dlVar            Identifier
hi def link dlType           Type
hi def link dlCst            Constant
hi def link dlString         String
hi def link dlMacro          Define
hi def link dlMacroInclude   Define
hi def link dlMacroIncluded  Constant
hi def link dlCppDelimiter   Special
hi def link dlCppSmallString Special
hi def link dlCppStringA     Special
hi def link dlCppStringB     Special
hi def link dlCppExt         Identifier
hi def link dlDepK           Keyword
hi def link dlTypeK          Keyword
hi def link dlTypeClause     Type
hi def link dlDependency     Identifier
hi def link dlGlobalK        Keyword
hi def link dlGlobal         Constant
hi def link dlGlobalVal      Special
hi def link dlGlobalType     Type
hi def link dlGoalK          Keyword
hi def link dlInputK         Keyword
hi def link dlInputCost      Conditional
hi def link dlInputAK        Conditional
hi def link dlReqK           Conditional
hi def link dlFilterK        Conditional
hi def link dlWriterK        Conditional
hi def link dlSetK           Conditional

let b:current_syntax = "parfait-datalog"

