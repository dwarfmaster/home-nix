
;; Theme
(setq doom-font "FuraCode Nerd Font Mono-11:weight=bold")
(setq doom-theme 'doom-city-lights)

;; Calendar config
(defun dwarfmaster/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :content-sources
   (list
    (cfx:org-create-source "Green"))))

;; Elfeed config
(setq rmh-elfeed-org-files (list "~/wiki/support/feeds.org"))
(after! elfeed
        (setq elfeed-search-filter "@1-month-ago +unread"))


;; Org mode
(setq dwarfmaster/agenda-files
      (list "~/wiki/index.org"
            "~/wiki/projects/"
            "~/wiki/support/"))

;; Choose what to unfold depending on jump method
;; See the documentation of the variable for more details on what that means
(setq org-show-context-detail
      '((agenda . local)
        (bookmark-jump . lineage)
        (isearch . ancestors)
        (occur-tree . local)
        (default . lineage)))
;; Increment integers when copying down
(setq org-table-copy-increment t)

(setq org-todo-keywords
      ;; Sequences for projects
      '((sequence "IDEA(!)" "TODO(!)" "PROBLEM(!)")
    (sequence "PROJECT(!)" "POSTPONNED(@)" "SUPPORT(!)")
    (sequence "|" "DISCARDED(@)" "COMPLETED(@)")
       ;; Sequences for tasks
    (sequence "CONSIDER(!)" "TASK(!)" "NEXT(!)")
    (sequence "STARTED(!)" "WAITING(@)" "PAUSED(@)")
    (sequence "|" "DONT(@)" "FAILED(@)" "DONE(!)")))

;; Warn for upcomming deadlines 7 days in advance
(setq org-deadline-warning-days 7)
;; Set org directory
(setq org-directory "~/data/annex/wiki")
;; Set the agenda files
(setq org-agenda-files dwarfmaster/agenda-files)
;; Display inline images
(setq org-startup-with-inline-images t)
;; Ask for confirmation before running babel, shell link or elisp link
(setq org-confirm-babel-evaluate t)
(setq org-confirm-elisp-link-function 'yes-or-no-p)
(setq org-confirm-shell-link-function 'yes-or-no-p)
;; Do not indent the content of header
(setq org-adapt-indentation nil)

;; Files to open with applications
(setq org-file-apps
      `((auto-mode . emacs)
        ;; Documents
        ("\\.pdf\\'"  . "xdg-open \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
        ;; Videos
        ("\\.mp4\\'"  . "xdg-open \"%s\"")
        ("\\.mkv\\'"  . "xdg-open \"%s\"")
        ;; Pictures
        ("\\.png\\'"  . "xdg-open \"%s\"")
        ("\\.jpg\\'"  . "xdg-open \"%s\"")
        ("\\.JPG\\'"  . "xdg-open \"%s\"")
        ("\\.jpeg\\'" . "xdg-open \"%s\"")
        ("\\.bmp\\'"  . "xdg-open \"%s\"")
        ("\\.gif\\'"  . "xdg-open \"%s\"")))

;; Start agenda view on sunday
(setq org-agenda-start-on-weekday 0)
;; Span of the agenda view
(setq org-agenda-span 'week)
;; Define stuck projects
(setq org-stuck-projects
      '("TODO=\"PROJECT\"" ("NEXT" "WAITING" "STARTED") nil "")) 
;; Define sorting strategy
(setq org-agenda-sorting-strategy
      ;; Show higher priority lower effort first, and them alphabetically
      ;; (to make it predictable)
      '(priority-down effort-up alpha-up))
;; Ignore scheduled, deadlined and tasks with timestamps in general
(setq org-agenda-todo-ignore-with-date t)

;; Define some custom views
(setq org-agenda-custom-commands
      '(("d" "Default view, show week and active tasks"
         ;; This one is the working view, used to know what task is next
     ;; and to schedule tasks
     ((agenda "")
      (todo "STARTED")
      (todo "NEXT")
      (todo "WAITING")
      (todo "CONSIDER")))
    ("i" "Show stuck projects, ideas, problems and todos"
     ;; This one should be empty after each review
     ((todo "TODO")
      (todo "IDEA")
      (todo "PROBLEM")
      (stuck)))
    ("p" "Show all projects and supports"
     ;; Overview of current and postponned projects
     ((todo "PROJECT")
      (todo "SUPPORT")
      (todo "POSTPONNED")))
    ("M" "Show all headers with mobile tag"
     tags "+mobile"
     ((org-use-tag-inheritance nil)))
    ))


;; Org Attach
(setq org-attach-directory "/data/luc/annex/data")
(setq org-attach-method 'mv)
(setq org-attach-auto-tag "attach")
;; Stores a link to the file when attaching it
(setq org-attach-store-link-p t)
(setq org-attach-archive-delete nil)
;; Ask before getting git annexed files
(setq org-attach-annex-auto-get 'ask)
;; Inherit DIR properties
(setq org-attach-allow-inheritance t)
;; Property used to list attached file, not necessary
(setq org-attach-file-list-property "attached")
;; Do not commit attachements with git ! Will use a special hook
(setq org-attach-commit nil)
;; Add support for attachement link
(after! org
  (push '("att" . org-attach-expand-link) org-link-abbrev-alist))


;; Org Roam
;; Use wiki as org roam main directory
(setq org-roam-directory "~/wiki")
;; Set the index file
(setq org-roam-index-file (concat org-roam-directory "/index.org"))
;; Store the database in $XDG_CACHE_HOME/org-roam/db.sqlite3
(setq org-roam-db-location (concat (getenv "XDG_CACHE_HOME")
                   "/org-roam/db.sqlite3"))
;; Default capture
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
    ))


;; Org capture
;; Set default notes file for capture
(setq org-default-notes-file (concat org-directory "/inbox.org"))
;; Always set the org-capture-last-stored bookmark
(setq org-capture-bookmark t)
;; Use template F when capturing from org-protocol
(setq org-protocol-default-template-key "F")

(defun dwarfmaster/format-html-title (title)
  "Change an arbitrary html title so that it can safely be inserted in an org link"
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) title)))
(defun dwarfmaster/capture/prepare-keys (keys)
  "Unhex URL"
  (mapcar #'(lambda (c) (if (char-or-string-p c) (url-unhex-string c) c))
      keys))
(defun dwarfmaster/capture/protocol (fun &rest rest)
  "Call org-protocol-capture with unhexed url"
  (apply fun (dwarfmaster/capture/prepare-keys (car rest)) (cdr rest)))
(advice-add 'org-protocol-capture :around #'dwarfmaster/capture/protocol)

(defun dwarfmaster/capture/build-roam-candidates ()
  "Return a completion list for helm with all org roam candidates"
  (mapcar (lambda (cand)
        `(,(car cand)
          . ,(concat "[[" (plist-get (cdr cand) :path)
             "][" (plist-get (cdr cand) :title)
             "]]")))
      (org-roam--get-title-path-completions)))

(defun dwarfmaster/capture/find-roam-note ()
  "Use helm to find a roam note and return a org link to it"
  (interactive)
  (helm :buffer "*helm capture note*"
    :sources (helm-build-sync-source "roam notes"
           :candidates (dwarfmaster/capture/build-roam-candidates))))

;; Templates
(setq org-capture-templates
      '(
    ;; Templates to use from emacs
    ("i" "Idea" entry (file+headline "" "Captured")
     "
** IDEA %^{Idea: } %^G
   %U"
     :empty-lines 1 :clock-resume :immediate-finish)
        ("t" "Todo" entry (file+headline "" "Captured")
     "
** TODO %^{Idea: } %^G
   %U"
     :empty-lines 1 :clock-resume :immediate-finish)
        ("P" "Problem" entry (file+headline "" "Captured")
     "
** PROBLEM %^{Idea: } %^G
   %U"
     :empty-lines 1 :clock-resume :immediate-finish)
    ("b" "book" entry (file+headline "" "Captured")
     "
*** %(dwarfmaster/capture/find-roam-note)
:PROPERTIES:
:STATUS: %^{Rating: }
:DIFFICULTY: %^{Difficulty: }
:END:"
     :empty-lines 1 :clock-resume :immediate-finish)
        ;; Templates for 
        ("F" "Protocol" entry (file+headline "" "Firefox")
     "
** [[%:link][%(dwarfmaster/format-html-title \"%:description\")]]
   %U

#+BEGIN_QUOTE
%:initial
#+END_QUOTE"
     :empty-lines 1)
    ("G" "Protocol link" entry (file+headline "" "Firefox")
     "** [[%:link][%(dwarfmaster/format-html-title \"%:description\")]]\n   %U"
     :empty-lines 1)
    ))

;; Org refile
;; Cache refile destinations
(setq org-refile-use-cache t)
;; Log timestamp when refiling entries
(setq org-log-refile 'time)
;; New notes (or refiled notes) are at the end
(setq org-reverse-note-order nil)
;; Set the targets for refiling
;; TODO improve selection using notdeft
(setq org-refile-targets
      '((nil . (:maxlevel . 2)) ; Up to level 2 in current file
    (org-agenda-files . (:maxlevel . 9))))
;; Better handling for multiple subheaders with same name, and allow refiling to top level
(setq org-refile-use-outline-path 'file)
;; Do dot complete path in steps
(setq org-outline-path-complete-in-steps nil)
