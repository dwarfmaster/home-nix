
(doom! :app
       calendar
       (rss +org)

       :checkers
       (syntax +childframe)
       (spell +everywhere +hunspell)
       grammar

       :completion
       (company +tng)
       (helm +fuzzy)

       :config
       (default +bindings +smartparens)

       :editor
       (evil +everywhere)
       file-templates
       fold
       lispy
       snippets

       :emacs
       (dired +icons +ranger)
       (ibuffer +icons)
       (undo +tree)
       
       :lang
       (cc +lsp)
       coq
       emacs-lisp
       (haskell +dante)
       idris
       (latex +latexmk +cdlatex +lsp)
       ledger
       nix
       ocaml
       (org +roam +pretty)
       rust
       sh
       web

       :term
       eshell

       :tools
       direnv
       (debugger +lsp)
       (eval +overlay)
       (lookup +dictionary +docsets)
       (lsp +peek)
       magit
       make
       (pass +auth)
       rgb

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       hydra
       modeline
       nav-flash
       neotree
       ophints
       (popup +all +defaults)
       window-select
       workspace
       )

