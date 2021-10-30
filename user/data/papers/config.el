
(load! "+nix.el")

;; Biblio
;;  ____  _ _     _ _
;; | __ )(_) |__ | (_) ___
;; |  _ \| | '_ \| | |/ _ \
;; | |_) | | |_) | | | (_) |
;; |____/|_|_.__/|_|_|\___/
(setq bibtex-completion-bibliography
      '("/home/luc/papers/references.bib"))
(setq bibtex-completion-pdf-field "file")
;; Open PDFs with zathura
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (call-process *nix/pdfreader* nil 0 nil fpath)))
