
;;; Code:

;;; Theme
 ;; _____ _
;; |_   _| |__   ___ _ __ ___   ___
  ;; | | | '_ \ / _ \ '_ ` _ \ / _ \
  ;; | | | | | |  __/ | | | | |  __/
  ;; |_| |_| |_|\___|_| |_| |_|\___|

(setq doom-font "FiraCode Nerd Font Mono-11:weight=bold")

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(solaire-global-mode +1)
(setq fancy-splash-image "~/wiki/logo.png")

(setq doom-theme 'doom-manegarm)
(after! doom-themes
  ;; Enable flashing modeline on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neatree theme
  (doom-themes-neotree-config)
  ;; Corrects org native fontification
  (doom-themes-org-config))

;;; Basic UI
(map! :leader "b" 'helm-mini)

;;; Elfeed
 ;; _____ _  __               _
;; | ____| |/ _| ___  ___  __| |
;; |  _| | | |_ / _ \/ _ \/ _` |
;; | |___| |  _|  __/  __/ (_| |
;; |_____|_|_|  \___|\___|\__,_|

(setq rmh-elfeed-org-files (list "~/wiki/support/feeds.org"))
(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +unread -webcomics"))
(map! :leader "o l" 'elfeed)

;;; Figlet
 ;; _____ _       _      _
;; |  ___(_) __ _| | ___| |_
;; | |_  | |/ _` | |/ _ \ __|
;; |  _| | | (_| | |  __/ |_
;; |_|   |_|\__, |_|\___|\__|
         ;; |___/
(defun dwarfmaster/make-figlet-text (b)
  "Interactively query a string from the user, and figletify it. B is a boolean indicating the size."
  (forward-line 1)
  (insert
    (shell-command-to-string
      (concat "figlet" (if b " -f small " " ") (read-string "Figlet: "))))
  )
(defun dwarfmaster/make-figlet-text-normal ()
  "Figletify a queried string."
  (interactive)
  (dwarfmaster/make-figlet-text nil))
(defun dwarfmaster/make-figlet-text-small ()
  "Figletify a queried string with small font."
  (interactive)
  (dwarfmaster/make-figlet-text t))

(map! :leader
      "i g"  'dwarfmaster/make-figlet-text-normal
      "i G"  'dwarfmaster/make-figlet-text-small)


;;; Org config
  ;; ___               ____             __ _
 ;; / _ \ _ __ __ _   / ___|___  _ __  / _(_) __ _
;; | | | | '__/ _` | | |   / _ \| '_ \| |_| |/ _` |
;; | |_| | | | (_| | | |__| (_) | | | |  _| | (_| |
 ;; \___/|_|  \__, |  \____\___/|_| |_|_| |_|\__, |
           ;; |___/                          |___/
(setq dwarfmaster/agenda-files
      (list "~/wiki/index.org"
            "~/wiki/projects/"
            "~/wiki/support/"))
;; Set org directory
(setq org-directory "~/data/annex/wiki")
(after! org
  ;; Set the agenda files
  (setq org-agenda-files dwarfmaster/agenda-files)
  (setq org-todo-keywords
      ;; Sequences for projects
    '((sequence "IDEA(!)" "TODO(!)" "PROBLEM(!)")
      (sequence "PROJECT(!)" "POSTPONNED(@)" "SUPPORT(!)")
      (sequence "|" "DISCARDED(@)" "COMPLETED(@)")
      ;; Sequences for tasks
      (sequence "CONSIDER(!)" "TASK(!)" "NEXT(!)")
      (sequence "STARTED(!)" "WAITING(@)" "PAUSED(@)")
      (sequence "|" "DONT(@)" "FAILED(@)" "DONE(!)")))
  ;; When dealing with repeated TODOs, don't repeat to previous task
  (setq org-todo-repeat-to-state nil)
  ;; Set default tags
  ;; TODO Set full list of tags
  (setq org-tag-alist
        '(("mobile" "fantasy" "scifi" "urban" "opensource" "movie")))
  ;; Warn for upcomming deadlines 7 days in advance
  (setq org-deadline-warning-days 7)
  ;; Archive in another file in current directory with .org_archive extension
  (setq org-archive-location "%s_archive::")
  ;; Files to open with applications
  (setq org-file-apps
        `((auto-mode . emacs)
          ;; Documents (not using xdg-open since it doesn't support open at page)
          ("\\.pdf\\'"  . "zathura \"%s\"")
          ("\\.pdf::\\([0-9]+\\)\\'" . "zathura \"%s\" -P %1")
          ("\\.epub\\'"  . "zathura \"%s\"")
          ;; Videos
          ("\\.mp4\\'"  . "xdg-open \"%s\"")
          ("\\.mkv\\'"  . "xdg-open \"%s\"")
          ("\\.avi\\'"  . "xdg-open \"%s\"")
          ;; Pictures
          ("\\.png\\'"  . "xdg-open \"%s\"")
          ("\\.jpg\\'"  . "xdg-open \"%s\"")
          ("\\.JPG\\'"  . "xdg-open \"%s\"")
          ("\\.jpeg\\'" . "xdg-open \"%s\"")
          ("\\.bmp\\'"  . "xdg-open \"%s\"")
          ("\\.gif\\'"  . "xdg-open \"%s\""))))


;;; Org UI
  ;; ___              _   _ ___
 ;; / _ \ _ __ __ _  | | | |_ _|
;; | | | | '__/ _` | | | | || |
;; | |_| | | | (_| | | |_| || |
 ;; \___/|_|  \__, |  \___/|___|
           ;; |___/
(after! org
  (setq org-todo-keyword-faces
        ;; Projects
        `(("IDEA" . (:foreground ,(doom-color 'base7)
                     :background ,(doom-color 'blue)
                     :weight bold))
          ("TODO" . (:foreground ,(doom-color 'base7)
                     :background ,(doom-color 'blue)
                     :weight bold))
          ("PROBLEM" . (:foreground ,(doom-color 'base7)
                        :background ,(doom-color 'blue)
                        :weight bold))
          ("PROJECT" . (:foreground ,(doom-color 'base7)
                        :background ,(doom-color 'violet)
                        :weight bold))
          ("POSTPONNED" . (:foreground ,(doom-color 'bg)
                           :background ,(doom-color 'blue)
                           :weight bold))
          ("SUPPORT" . (:foreground ,(doom-color 'violet)
                        :background ,(doom-color 'base7)
                        :weight bold))
          ("DISCARDED" . (:foreground ,(doom-color 'base7)
                          :background ,(doom-color 'orange)
                          :weight bold))
          ("COMPLETED" . (:foreground ,(doom-color 'violet)
                          :background ,(doom-color 'base7)
                          :weight bold))

          ;; Tasks
          ("CONSIDER" . (:foreground ,(doom-color 'yellow)
                         :background ,(doom-color 'base7)
                         :weight bold))
          ("TASK" . (:foreground ,(doom-color 'orange)
                     :background ,(doom-color 'base7)
                     :weight bold))
          ("NEXT" . (:foreground ,(doom-color 'green)
                     :background ,(doom-color 'base7)
                     :weight bold))
          ("STARTED" . (:foreground ,(doom-color 'base7)
                        :background ,(doom-color 'green)
                        :weight bold))
          ("WAITING" . (:foreground ,(doom-color 'green)
                        :background ,(doom-color 'base7)
                        :weight bold))
          ("PAUSED" . (:foreground ,(doom-color 'green)
                       :background ,(doom-color 'base7)
                       :weight bold))
          ("DONT" . (:foreground ,(doom-color 'red)
                     :background ,(doom-color 'base7)
                     :weight bold))
          ("FAILED" . (:foreground ,(doom-color 'base7)
                       :background ,(doom-color 'red)
                       :weight bold))
          ("DONE" . (:foreground ,(doom-color 'base7)
                     :background ,(doom-color 'green)
                     :weight bold))))
  ;; Ask for confirmation before running babel, shell link or elisp link
  (setq org-confirm-babel-evaluate t)
  (setq org-confirm-elisp-link-function 'yes-or-no-p)
  (setq org-confirm-shell-link-function 'yes-or-no-p)
  )


;; Task switcher
;; _____         _      ____          _ _       _
;; |_   _|_ _ ___| | __ / ___|_      _(_) |_ ___| |__   ___ _ __
;; | |/ _` / __| |/ / \___ \ \ /\ / / | __/ __| '_ \ / _ \ '__|
;; | | (_| \__ \   <   ___) \ V  V /| | || (__| | | |  __/ |
;; |_|\__,_|___/_|\_\ |____/ \_/\_/ |_|\__\___|_| |_|\___|_|

(defmacro dwarfmaster/org/make-todo-switcher (todo)
  "Create a function dwarfmaster/org/todo-swith-%todo that switches
  the header to the right todo"
  (let ((fname (intern (concat "dwarfmaster/org/todo-switch-" (downcase todo)))))
    `(defun ,fname ()
       ,(concat "Switch current header to TODO state " todo)
       (interactive)
       (org-todo ,todo))))
;; Projects
(dwarfmaster/org/make-todo-switcher "IDEA")
(dwarfmaster/org/make-todo-switcher "TODO")
(dwarfmaster/org/make-todo-switcher "PROBLEM")
(dwarfmaster/org/make-todo-switcher "PROJECT")
(dwarfmaster/org/make-todo-switcher "POSTPONNED")
(dwarfmaster/org/make-todo-switcher "SUPPORT")
(dwarfmaster/org/make-todo-switcher "DISCARDED")
(dwarfmaster/org/make-todo-switcher "COMPLETED")
;; Tasks
(dwarfmaster/org/make-todo-switcher "CONSIDER")
(dwarfmaster/org/make-todo-switcher "TASK")
(dwarfmaster/org/make-todo-switcher "NEXT")
(dwarfmaster/org/make-todo-switcher "STARTED")
(dwarfmaster/org/make-todo-switcher "WAITING")
(dwarfmaster/org/make-todo-switcher "PAUSED")
(dwarfmaster/org/make-todo-switcher "DONT")
(dwarfmaster/org/make-todo-switcher "FAILED")
(dwarfmaster/org/make-todo-switcher "DONE")

;; Tasks
(defhydra dwarfmaster/hydra/org/todo/tasks (:color blue :hint nil)
  "
Task switcher

^Starting^        ^Continuing^      ^Stopping
^^^^^^------------------------------------------
[_c_] Consider    [_s_] Started     [_d_] Done
[_t_] Task        [_w_] Waiting     [_f_] Failed
[_n_] Next        [_p_] Paused      [_D_] Don't

[_!_] Switch to projects
"
  ("c"    dwarfmaster/org/todo-switch-consider)
  ("t"    dwarfmaster/org/todo-switch-task)
  ("n"    dwarfmaster/org/todo-switch-next)
  ("s"    dwarfmaster/org/todo-switch-started)
  ("w"    dwarfmaster/org/todo-switch-waiting)
  ("p"    dwarfmaster/org/todo-switch-paused)
  ("d"    dwarfmaster/org/todo-switch-done)
  ("f"    dwarfmaster/org/todo-switch-failed)
  ("D"    dwarfmaster/org/todo-switch-dont)
  ("!"    dwarfmaster/hydra/org/todo/projects/body)
  ("<escape>"  nil :color blue)
  )
;; Projects
(defhydra dwarfmaster/hydra/org/todo/projects (:color blue :hint nil)
  "
Project switcher

^Starting^        ^Continuing^      ^Stopping
^^^^^^---------------------------------------------
[_i_] Idea        [_p_] Project     [_c_] Completed
[_t_] Todo        [_P_] Postponned  [_d_] Discarded
[_r_] Problem

[_!_] Switch to tasks
"
  ("i"     dwarfmaster/org/todo-switch-idea)
  ("t"     dwarfmaster/org/todo-switch-todo)
  ("r"     dwarfmaster/org/todo-switch-problem)
  ("p"     dwarfmaster/org/todo-switch-project)
  ("P"     dwarfmaster/org/todo-switch-postponned)
  ("c"     dwarfmaster/org/todo-switch-completed)
  ("d"     dwarfmaster/org/todo-switch-discarded)
  ("!"     dwarfmaster/hydra/org/todo/tasks/body)
  ("<escape>"  nil :color blue)
  )

(defun dwarfmaster/org/todo-switch-dwim ()
  "Open the right hydra depending on the sequence at point."
  (interactive)
  (let ((todo-at-point (org-get-todo-state))
    (tasks-todos    '("CONSIDER" "TASK" "NEXT"
                      "STARTED" "WAITING" "PAUSED"
                      "DONE" "FAILED" "DONT"))
    (projects-todos '("IDEA" "TODO" "PROBLEM"
                      "PROJECT" "POSTPONNED"
                      "COMPLETED" "DISCARDED")))
    (cond
      ((member todo-at-point tasks-todos)
         (dwarfmaster/hydra/org/todo/tasks/body))
      ((member todo-at-point projects-todos)
         (dwarfmaster/hydra/org/todo/projects/body))
      (t (dwarfmaster/hydra/org/todo/tasks/body)))))

(map! :after org :map org-mode-map
      :localleader "!" 'dwarfmaster/org/todo-switch-dwim)

;; Org Agenda
  ;; ___                 _                        _
 ;; / _ \ _ __ __ _     / \   __ _  ___ _ __   __| | __ _
;; | | | | '__/ _` |   / _ \ / _` |/ _ \ '_ \ / _` |/ _` |
;; | |_| | | | (_| |  / ___ \ (_| |  __/ | | | (_| | (_| |
 ;; \___/|_|  \__, | /_/   \_\__, |\___|_| |_|\__,_|\__,_|
           ;; |___/          |___/
(after! org
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
      )))

(defun dwarfmaster/agenda/default ()
  "Open the default weekly agenda view."
  (interactive)
  (org-agenda nil "d"))
(defun dwarfmaster/agenda/review ()
  "Open the agenda view with things to review."
  (interactive)
  (org-agenda nil "i"))
(defun dwarfmaster/agenda/projects ()
  "Open the agenda view with project overview."
  (interactive)
  (org-agenda nil "p"))
(defun dwarfmaster/agenda/mobile ()
  "Open the agenda view with mobile headers."
  (interactive)
  (org-agenda nil "M"))

(map! :leader
      "o a" 'dwarfmaster/agenda/default
      "o i" 'dwarfmaster/agenda/review
      "o p" 'dwarfmaster/agenda/projects
      "o m" 'dwarfmaster/agenda/mobile)

;; Org Attach
  ;; ___                 _   _   _             _
 ;; / _ \ _ __ __ _     / \ | |_| |_ __ _  ___| |__
;; | | | | '__/ _` |   / _ \| __| __/ _` |/ __| '_ \
;; | |_| | | | (_| |  / ___ \ |_| || (_| | (__| | | |
 ;; \___/|_|  \__, | /_/   \_\__|\__\__,_|\___|_| |_|
           ;; |___/
(after! org
  (setq org-attach-directory "/data/luc/annex/data")
  (setq org-attach-method 'mv)
  (setq org-attach-auto-tag "attach")
  ;; Stores a link to the file when attaching it
  (setq org-attach-store-link-p t)
  (setq org-attach-archive-delete nil)
  ;; Ask before getting git annexed files
  (setq org-attach-annex-auto-get 'ask)
  ;; Do not commit attachements with git ! Will use a special hook
  (setq org-attach-commit nil))


;; Org Roam
  ;; ___              ____
 ;; / _ \ _ __ __ _  |  _ \ ___   __ _ _ __ ___
;; | | | | '__/ _` | | |_) / _ \ / _` | '_ ` _ \
;; | |_| | | | (_| | |  _ < (_) | (_| | | | | | |
 ;; \___/|_|  \__, | |_| \_\___/ \__,_|_| |_| |_|
           ;; |___/
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

;; Biblio
 ;; ____  _ _     _ _
;; | __ )(_) |__ | (_) ___
;; |  _ \| | '_ \| | |/ _ \
;; | |_) | | |_) | | | (_) |
;; |____/|_|_.__/|_|_|\___/
;; TODO


;; Mobile sync
 ;; __  __       _     _ _        ____
;; |  \/  | ___ | |__ (_) | ___  / ___| _   _ _ __   ___
;; | |\/| |/ _ \| '_ \| | |/ _ \ \___ \| | | | '_ \ / __|
;; | |  | | (_) | |_) | | |  __/  ___) | |_| | | | | (__
;; |_|  |_|\___/|_.__/|_|_|\___| |____/ \__, |_| |_|\___|
                                     ;; |___/
;; TODO


;; Webcomics Summary
;; __        __   _                         _
;; \ \      / /__| |__   ___ ___  _ __ ___ (_) ___
 ;; \ \ /\ / / _ \ '_ \ / __/ _ \| '_ ` _ \| |/ __|
  ;; \ V  V /  __/ |_) | (_| (_) | | | | | | | (__
   ;; \_/\_/ \___|_.__/ \___\___/|_| |_| |_|_|\___|

 ;; ____
;; / ___| _   _ _ __ ___  _ __ ___   __ _ _ __ _   _
;; \___ \| | | | '_ ` _ \| '_ ` _ \ / _` | '__| | | |
 ;; ___) | |_| | | | | | | | | | | | (_| | |  | |_| |
;; |____/ \__,_|_| |_| |_|_| |_| |_|\__,_|_|   \__, |
                                            ;; |___/
(defun dwarfmaster/elfeed/clean-title (title)
  "Replace all | by - in TITLE."
  (replace-regexp-in-string "|" "-" title))

(defun dwarfmaster/elfeed/prepare-webcomic (feed result)
  "Add basic information associated to FEED in RESULT."
  (let ((id    (elfeed-feed-id feed))
        (title (dwarfmaster/elfeed/clean-title (elfeed-feed-title feed))))
    (when (not (assoc id result))
      (push `(,id . ,(list title nil 0)) result))
    result))

(defun dwarfmaster/elfeed/add-unread (feed result)
  "Increment by one the unread count associated to FEED in RESULT."
  (let* ((id     (elfeed-feed-id feed))
         (rentry (alist-get id result)))
    (setf (nth 2 rentry) (+ 1 (nth 2 rentry)))
    (setf (alist-get id result) rentry)))

(defun dwarfmaster/elfeed/set-last-read (feed entry result)
  "If the last read for FEED is not set, set it to ENTRY in RESULT."
  (let* ((id     (elfeed-feed-id feed))
         (lread  (nth 1 (alist-get id result)))
         (rentry (alist-get id result)))
    (when (not lread)
      (setf (nth 1 rentry) (elfeed-entry-link entry))
      (setf (alist-get id result) rentry))))

(defun dwarfmaster/elfeed/webcomics (tag)
  "Return webcomics name, last read and number of unreads from TAG."
  (let ((result nil))
    (with-elfeed-db-visit (entry feed)
      (let* ((tags        (elfeed-entry-tags entry))
             (is-webcomic (and (member 'webcomics tags)
                               (member tag        tags)))
             (is-unread   (member 'unread tags)))
        (when is-webcomic
          (setq result (dwarfmaster/elfeed/prepare-webcomic feed result))
          (if is-unread
              (dwarfmaster/elfeed/add-unread feed result)
            (dwarfmaster/elfeed/set-last-read feed entry result)))))
    result))

(defun dwarfmaster/elfeed/mark-feed-as-read (feed-id)
  "Mark all entries of the feed identified by FEED-ID as read."
  (let ((entries (elfeed-feed-entries feed-id)))
    (elfeed-untag entries 'unread)))

(defun dwarfmaster/elfeed/pp-row (row)
  "Pretty print a ROW."
  (let* ((id      (car row))
         (title   (cadr row))
         (last    (nth 1 (cdr row)))
         (unreads (nth 2 (cdr row))))
    (format-message "| [[%s][%s]] | %d | [[elisp:(dwarfmaster/elfeed/mark-feed-as-read \"%s\")][Mark as read]] |\n"
                    last title
                    unreads
                    id)))

(defun dwarfmaster/elfeed/webcomics-summary (tag)
  "Make summary of webcomics with TAG."
  (let* ((webs (sort (dwarfmaster/elfeed/webcomics tag)
                     (lambda (x y) (< (nth 2 (cdr y))
                                      (nth 2 (cdr x)))))))
    (concat "|--|\n| Webcomic | Unreads | Action |\n|--|\n"
            (apply 'concat (mapcar 'dwarfmaster/elfeed/pp-row webs))
            "|--|\n")))

(defun org-dblock-write:webcomics-summary (params)
  (let* ((tag     (or (plist-get params :tag) 'webcomics))
         (summary (dwarfmaster/elfeed/webcomics-summary tag)))
    (insert summary)
    (org-table-align)))

;; Language: ledger
 ;; _             _
;; | |    ___  __| | __ _  ___ _ __
;; | |   / _ \/ _` |/ _` |/ _ \ '__|
;; | |__|  __/ (_| | (_| |  __/ |
;; |_____\___|\__,_|\__, |\___|_|
                 ;; |___/

;; TODO move to hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))
;; Configure ledger-mode to work with hledger
(setq ledger-mode-should-check-version nil
      ledger-report-links-in-register nil
      ledger-binary-path "hledger")

;; Spell checker
(after! ispell
  (setq ispell-local-dictionary-alist
        '(("en_US"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US,en_CA,en_AU,fr-toutesvariantes")
           nil
           iso-8859-1)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
  (setq ispell-dictionary "en_US")
  (setq ispell-personal-dictionary (concat (getenv "XDG_CACHE_HOME") "/hunspell/personal")))

;; Dedukti
;;  ____           _       _    _   _
;; |  _ \  ___  __| |_   _| | _| |_(_)
;; | | | |/ _ \/ _` | | | | |/ / __| |
;; | |_| |  __/ (_| | |_| |   <| |_| |
;; |____/ \___|\__,_|\__,_|_|\_\\__|_|
;;

(use-package! company :after lambdapi-mode)
(use-package! company-math :after lambdapi-mode)
(map! :after lambdapi-mode
      :map lambdapi-mode-map
      :localleader
      "]" #'lp-proof-forward
      "[" #'lp-proof-backward
      "." #'lp-prove-till-cursor
      ";" #'comment-dwim
      "r" #'lambdapi-refresh-window-layout
      (:prefix ("g" . "goto")
       "n" #'lp-jump-proof-forward
       "p" #'lp-jump-proof-backward))
