
;;; Org config
;;   ___               ____             __ _
;;  / _ \ _ __ __ _   / ___|___  _ __  / _(_) __ _
;; | | | | '__/ _` | | |   / _ \| '_ \| |_| |/ _` |
;; | |_| | | | (_| | | |__| (_) | | | |  _| | (_| |
;;  \___/|_|  \__, |  \____\___/|_| |_|_| |_|\__, |
;;            |___/                          |___/
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

;; Org Id
;;   ___              ___ ____
;;  / _ \ _ __ __ _  |_ _|  _ \
;; | | | | '__/ _` |  | || | | |
;; | |_| | | | (_| |  | || |_| |
;;  \___/|_|  \__, | |___|____/
;;            |___/

(defun dwarfmaster/org-id-refresh-extra-files ()
  "Update org-id-extra-files if org files where created or deleted in the wiki."
  (interactive)
  (setq org-id-extra-files (directory-files-recursively "/data/luc/annex/wiki" "\.org$")))

(after! org
  (setq org-id-track-globally t)
  (dwarfmaster/org-id-refresh-extra-files))
(map! :after org :map org-mode-map :localleader
      "E" 'dwarfmaster/org-id-refresh-extra-files)

;; HUGO Export
;;  _   _ _   _  ____  ___    _____                       _
;; | | | | | | |/ ___|/ _ \  | ____|_  ___ __   ___  _ __| |_
;; | |_| | | | | |  _| | | | |  _| \ \/ / '_ \ / _ \| '__| __|
;; |  _  | |_| | |_| | |_| | | |___ >  <| |_) | (_) | |  | |_
;; |_| |_|\___/ \____|\___/  |_____/_/\_\ .__/ \___/|_|   \__|
;;                                      |_|

(defun dwarfmaster/org-roam-export-all ()
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (dolist (f (org-roam-list-files))
    (with-current-buffer (find-file f)
      (org-hugo-export-wim-to-md))))
(map! :leader
      "n E" 'dwarfmaster/org-roam-export-all)
(after! org
  ;; Never execute code blocks when exporting
  (setq org-export-use-babel nil))

;; Task switcher
;;  _____         _      ____          _ _       _
;; |_   _|_ _ ___| | __ / ___|_      _(_) |_ ___| |__   ___ _ __
;;   | |/ _` / __| |/ / \___ \ \ /\ / / | __/ __| '_ \ / _ \ '__|
;;   | | (_| \__ \   <   ___) \ V  V /| | || (__| | | |  __/ |
;;   |_|\__,_|___/_|\_\ |____/ \_/\_/ |_|\__\___|_| |_|\___|_|

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
;;   ___                 _                        _
;;  / _ \ _ __ __ _     / \   __ _  ___ _ __   __| | __ _
;; | | | | '__/ _` |   / _ \ / _` |/ _ \ '_ \ / _` |/ _` |
;; | |_| | | | (_| |  / ___ \ (_| |  __/ | | | (_| | (_| |
;;  \___/|_|  \__, | /_/   \_\__, |\___|_| |_|\__,_|\__,_|
;;            |___/          |___/
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
