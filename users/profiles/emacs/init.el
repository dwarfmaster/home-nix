
(doom! :app
       calendar
       (rss +org)

       :checkers
       (syntax +childframe)
       (spell +flyspell +everywhere +hunspell)
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
       vc
       
       :lang
       (agda +local)
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
       ;; eshell

       :tools
       biblio
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
       (emoji +github +unicode)
       hl-todo
       hydra
       (ligatures +extra +fira)
       modeline
       nav-flash
       neotree
       ophints
       (popup +all +defaults)
       window-select
       workspace
       )

