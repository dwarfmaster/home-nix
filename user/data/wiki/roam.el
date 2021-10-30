
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
(setq org-roam-db-location (concat (getenv "XDG_CACHE_HOME")
                   "/org-roam/doom.sqlite3"))
;; Default capture
(after! org
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "notes/%<%Y-%m>/%<%d_%H-%M-%S>-${slug}"
           :head "#+TITLE: ${title}\n\n"
           :unnarrowed t)
          ("p" "project" plain (function org-roam--capture-get-point)
           "* PROJECT ${title}
%u
%?"
           :file-name "projects/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_TAGS: project\n\n"
           :unnarrowed t)
          ("s" "support" plain (function org-roam--capture-get-point)
           "* SUPPORT ${title}
%u
%?"
           :file-name "support/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_TAGS: project\n\n"
           :unnarrowed t)
          )))
