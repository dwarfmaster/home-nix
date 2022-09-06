
(load! "+nix")

;; Org Roam
;;   ___              ____
;;  / _ \ _ __ __ _  |  _ \ ___   __ _ _ __ ___
;; | | | | '__/ _` | | |_) / _ \ / _` | '_ ` _ \
;; | |_| | | | (_| | |  _ < (_) | (_| | | | | | |
;;  \___/|_|  \__, | |_| \_\___/ \__,_|_| |_| |_|
;;            |___/
;; Use wiki as org roam main directory
(setq org-roam-directory "~/wiki")
;; Set the index file
(setq org-roam-index-file (concat org-roam-directory "/index.org"))
;; Store the database in $XDG_CACHE_HOME/org-roam/db.sqlite3
(setq org-roam-db-location (concat *nix/xdg-cache* "/org-roam/doom.sqlite3"))
;; Default capture
(after! org
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "notes/%<%Y-%m>/%<%d_%H-%M-%S>-${slug}.org"
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t)
          ("p" "project" plain
           "* PROJECT ${title}
%u
%?"
           :target (file+head "projects/${slug}.org"
                              "#+TITLE: ${title}\n#+ROAM_TAGS: project\n\n")
           :unnarrowed t)
          ("s" "support" plain
           "* SUPPORT ${title}
%u
%?"
           :target (file+head "support/${slug}.org"
                              "#+TITLE: ${title}\n#+ROAM_TAGS: project\n\n")
           :unnarrowed t)
          )))