
(map! :after blamer
      :leader
      "v i" 'blamer-show-commit-info
      "t B" 'blamer-mode)
(after! blamer
  (setq blamer-idle-time 1)
  (setq blamer-min-offset 30)
  (setq blamer-view 'overlay)
  (setq blamer-author-formatter " ✎ %s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s"))
;; Disable git blamer on org files
(define-global-minor-mode dwarfmaster/blamer blamer-mode
  (lambda ()
    (when (not (memq major-mode
                     '(org-mode)))
      (blamer-mode 1))))
(dwarfmaster/blamer 1)
