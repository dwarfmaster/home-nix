
(map! :after blamer
      :leader
      "v i" 'blamer-show-commit-info
      "t B" 'blamer-mode)
(after! blamer
  (setq blamer-idle-time 0.3)
  (setq blamer-min-offset 0.7)
  (setq blamer-view 'overlay)
  (setq blamer-author-formatter " ✎ %s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s"))
(global-blamer-mode 1)
