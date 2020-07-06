
;; TODO bind ESC to keyboard-quit in all buffers 

;; TRAMP
(require 'tramp)

;; Start the emacs server
(server-start)

;; Load nix constants
(require 'nixpaths)

;; Unbind normal keys
; TODO improve logic
(defun dwarfmaster/select-normal-and-unbind (map key value)
  "If key is alphanumeric, unbind it from map"
  (if (and (number-or-marker-p key)
	   (>= key 0)
	   (<= key 255))
      (define-key (symbol-value map) (byte-to-string key) nil)))

(defun dwarfmaster/unbind-normal-keys (map)
  "Select alphanumeric keys and unbind them from map"
  (map-keymap '(lambda (key value)
		 (dwarfmaster/select-normal-and-unbind map key value))
	      (symbol-value map)))

;; Color scheme, using the base16 package
;; https://github.com/belak/base16-emacs
(require 'base16-theme)
(load-theme 'base16-woodland t)
(defvar dwarfmaster/colors base16-woodland-colors)
(defvar dwarfmaster/c0     (plist-get dwarfmaster/colors :base00))
(defvar dwarfmaster/c1     (plist-get dwarfmaster/colors :base01))
(defvar dwarfmaster/c2     (plist-get dwarfmaster/colors :base02))
(defvar dwarfmaster/c3     (plist-get dwarfmaster/colors :base03))
(defvar dwarfmaster/c4     (plist-get dwarfmaster/colors :base04))
(defvar dwarfmaster/c5     (plist-get dwarfmaster/colors :base05))
(defvar dwarfmaster/c6     (plist-get dwarfmaster/colors :base06))
(defvar dwarfmaster/c7     (plist-get dwarfmaster/colors :base07))
(defvar dwarfmaster/c8     (plist-get dwarfmaster/colors :base08))
(defvar dwarfmaster/c9     (plist-get dwarfmaster/colors :base09))
(defvar dwarfmaster/ca     (plist-get dwarfmaster/colors :base0A))
(defvar dwarfmaster/cb     (plist-get dwarfmaster/colors :base0B))
(defvar dwarfmaster/cc     (plist-get dwarfmaster/colors :base0C))
(defvar dwarfmaster/cd     (plist-get dwarfmaster/colors :base0D))
(defvar dwarfmaster/ce     (plist-get dwarfmaster/colors :base0E))
(defvar dwarfmaster/cf     (plist-get dwarfmaster/colors :base0F))

;; Library to deal with timestamps
(require 'ts)

(defun dwarfmaster/ts/range-expand-days (start end inter &optional filter hour)
  "Expand a timestamp range [start; end], filtering out some dates"
  (let* ((filterf (if filter filter (lambda (ts) t)))
 	 (startTS (ts-parse-org start))
	 (endTS   (ts-parse-org end))
	 (current startTS)
	 (output  ""))
    (while (ts<= current endTS)
      (if (funcall filterf current)
	  (setq output
		(format "%s<%04d-%02d-%02d %s%s>\n"
			output
			(ts-year current)
			(ts-month current)
			(ts-day-of-month-num current)
			(ts-day-of-week-abbr current)
			(if hour (format " %02d:%02d"
					 (ts-hour current)
					 (ts-minute current)) ""))))
      (setq current (ts-adjust 'day inter current)))
    output))
;; (dwarfmaster/ts/range-expand-days "<2015-08-31 Mon 08:00>" "<2015-10-06 Tue>" 7)

;;; Vim emulation
;; __     ___           
;; \ \   / (_)_ __ ___  
;;  \ \ / /| | '_ ` _ \ 
;;   \ V / | | | | | | |
;;    \_/  |_|_| |_| |_|
;;                      

;; evil-mode, the main vim emulation in emacs
;; Find the documentation here https://evil.readthedocs.io/en/latest/index.html
(require 'evil)
(evil-mode 1)

;; Remove RET and SPC from motion keys, so that other modes can overload them
(require 'general)
(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "SPC"))
(general-unbind 'normal "SPC")
(general-unbind 'normal "RET")

;; Evil surround, implement support for handling surrounding characters
;; See https://github.com/emacs-evil/evil-surround for details
(require 'evil-surround)
(global-evil-surround-mode 1)
; Make sure to start all buffers in evil mode
(setq evil-emacs-state-modes '())


;; Autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Miscellaneous features
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; Figlet integration
;  ___ _      _     _   
; | __(_)__ _| |___| |_ 
; | _|| / _` | / -_)  _|
; |_| |_\__, |_\___|\__|
;       |___/           
(defun dwarfmaster/make-figlet-text (b)
  "Interactively query a string from the user, and figletify it"
  (forward-line 1)
  (insert
    (shell-command-to-string
      (concat nix/figlet (if b " -f small " " ") (read-string "Figlet: "))))
  )
(defun dwarfmaster/make-figlet-text-normal ()
  "Figletify a queried string"
  (interactive)
  (dwarfmaster/make-figlet-text nil))
(defun dwarfmaster/make-figlet-text-small ()
  "Figletify a queried string with small font"
  (interactive)
  (dwarfmaster/make-figlet-text t))
; TODO Add visual mode support to figletify selected string


;; General
;   ____                           _ 
;  / ___| ___ _ __   ___ _ __ __ _| |
; | |  _ / _ \ '_ \ / _ \ '__/ _` | |
; | |_| |  __/ | | |  __/ | | (_| | |
;  \____|\___|_| |_|\___|_|  \__,_|_|
;                                    
;; Set file for saving customs
(setq custom-file "~/workflow/config/emacs-custom.el")
(load custom-file)

;; Helm, ie navigation
;  _  _     _       
; | || |___| |_ __  
; | __ / -_) | '  \ 
; |_||_\___|_|_|_|_|
; https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

; Use fuzzy match everywhere
(setq helm-M-x-fuzzy-match        t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      )

(when (executable-find nix/curl)
  (setq helm-google-suggest-use-curl t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window
      helm-move-to-line-cycle-in-source     t ; cycle when reaching top or bottom in suggestions
      helm-ff-search-library-in-sexp        t ; search for library in `require` and `declare-function` sexp
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t
      )

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

; Fix a visual glitch in Helm due to evil
(defun spacemacs//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))
(add-hook 'helm-after-initialize-hook 
          'spacemacs//hide-cursor-in-helm-buffer)

; Evil compatibility : use helm with :b and :e
(define-key evil-ex-map "b " 'helm-mini)
(define-key evil-ex-map "e" 'helm-find-files)

(setq helm-autoresize-max-height 50)
(setq helm-autoresize-min-height 50)
(helm-autoresize-mode 1)

(helm-mode 1)


;; Projectile, ie project managment
;  ___          _        _   _ _     
; | _ \_ _ ___ (_)___ __| |_(_) |___ 
; |  _/ '_/ _ \| / -_) _|  _| | / -_)
; |_| |_| \___// \___\__|\__|_|_\___|
;            |__/                    
(require 'projectile)

; Tell projectile to use system tools instead of lisp implementation
; for indexing features
(setq projectile-indexing-method 'alien)
; Disable caching, since it will be using the local tools to index, and
; those will do more clever caching
(setq projectile-enable-caching nil)
; Only enable projectile on project with project root
(setq projectile-require-project-root t)
; Speed up by caching project files
(setq projectile-current-project-on-switch t)
; Prefix on modeline
(setq projectile-mode-line-prefix "Prj")
; Indicate dynamic information about project on modeline
(setq projectile-dynamic-mode-line t)

; Configure which extension to jump to
(setq projectile-other-file-alist
      '(("cpp" "hpp" "h")
	("hpp" "h" "cpp")
	("h" "c" "cpp" "hpp")
	("c" "h" "hpp")
	("vert" "frag")
	("frag" "vert")
	))


(projectile-mode +1)

(require 'helm-projectile)
; A very complete tutorial for helm integration with projectile here
;    https://tuhdo.github.io/helm-projectile.html


;; Magit, for git integration
;  __  __           _ _   
; |  \/  |__ _ __ _(_) |_ 
; | |\/| / _` / _` | |  _|
; |_|  |_\__,_\__, |_|\__|
;             |___/       
(require 'magit)
; Enable key mapping in non-magit buffers
(setq global-magit-file-mode t)
; Make magit-diff-buffer-file use a dedicated buffer
(setq magit-diff-buffer-file-locked t)
; Make magit-log-buffer-file use a dedicated buffer
(setq magit-log-buffer-file-locked t)
; Make magit commit uncommited changes to a wip branch
(setq magit-wip-mode t)
; Set the wip namespace
(setq magit-wip-namespace "dwarfmaster/wip/")
; Refresh magit status window when saving files
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
; Save all buffers editing repository files before executing any git command
(setq magit-save-repository-buffers 'dontask)
; Automatically updates files in buffer when the file changed on disk
(setq magit-auto-revert-mode t)
; Use file notifications to detect files to revert
(setq auto-revert-use-notify t)
; Make sure to report when reverting files
(setq auto-revert-verbose t)






;;; Wiki
;; __        ___ _    _ 
;; \ \      / (_) | _(_)
;;  \ \ /\ / /| | |/ / |
;;   \ V  V / | |   <| |
;;    \_/\_/  |_|_|\_\_|
;;                      

;; TODO get a more recent org mode and enable dynamic header numbering

;; When doing an edit commant on an invisible region, make it visible and only
;; do the edit if it feels predictible
(setq org-catch-invisible-edits 'smart)
;; Hide block by default when opening a new file
(setq org-hide-block-startup t)
;; Never split line when using meta ret
(setq org-M-RET-may-split-line '((default . nil)))
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
;; TODO setup some standard link abbreviations :
;;    https://orgmode.org/manual/Link-Abbreviations.html#Link-Abbreviations

(setq org-todo-keywords
      ;; Sequences for projects
      '((sequence "IDEA(!)" "TODO(!)" "PROBLEM(!)")
	(sequence "PROJECT(!)" "POSTPONNED(@)" "SUPPORT(!)")
	(sequence "|" "DISCARDED(@)" "COMPLETED(@)")
       ;; Sequences for tasks
	(sequence "CONSIDER(!)" "TASK(!)" "NEXT(!)")
	(sequence "STARTED(!)" "WAITING(@)" "PAUSED(@)")
	(sequence "|" "DONT(@)" "FAILED(@)" "DONE(!)")))
;; Colors and decorations for stages of TODO
(setq org-todo-keyword-faces
        ;; Projects
      `(("IDEA"       . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/cd
			 :weight bold))
        ("TODO"       . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/cd
			 :weight bold))
        ("PROBLEM"    . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/cd
			 :weight bold))
        ("PROJECT"    . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/ce
			 :weight bold))
        ("POSTPONNED" . (:foreground ,dwarfmaster/c0
			 :background ,dwarfmaster/cf
			 :weight bold))
        ("SUPPORT"    . (:foreground ,dwarfmaster/ce
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("DISCARDED"  . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/c9
			 :weight bold))
        ("COMPLETED"  . (:foreground ,dwarfmaster/ce
			 :background ,dwarfmaster/c2
			 :weight bold))

	;; Tasks
        ("CONSIDER"   . (:foreground ,dwarfmaster/ca
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("TASK"       . (:foreground ,dwarfmaster/c9
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("NEXT"       . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/c9
			 :weight bold))
        ("STARTED"    . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/cb
			 :weight bold))
        ("WAITING"    . (:foreground ,dwarfmaster/cb
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("PAUSED"     . (:foreground ,dwarfmaster/cb
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("DONT"       . (:foreground ,dwarfmaster/c8
			 :background ,dwarfmaster/c2
			 :weight bold))
        ("FAILED"     . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/c8
			 :weight bold))
        ("DONE"       . (:foreground ,dwarfmaster/c2
			 :background ,dwarfmaster/cc
			 :weight bold))))
;; When dealing with repeated TODOs, repeat to previous task when marked as done
(setq org-todo-repeat-to-state t)

;; When couting subtasks, count all of them and not just direct ones
(setq org-hierarchical-todo-statistics nil)
;; The list of usual tags
;; TODO fill it, and describe the tags
;; Tags can also be grouped in a hierarchical manner
(setq org-tag-alist '((:startgroup . nil) ; Start a group of mutually exclusive tags
		      ("@work" . ?w) ("@home" . ?h) ("hackens" . ?k)
		      (:endgroup . nil)
		      ("internship-brisbane" . ?B) ("internship-montreal" . ?M)))
;; Warn for upcomming deadlines 7 days in advance
(setq org-deadline-warning-days 7)
;; Save clock histroy across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; Ask for clock resolution after 15 minutes of idleness
(setq org-clock-idle-time 15)
;; Archive in another file in current directory with .org_archive extension
(setq org-archive-location "%s_archive::")
;; Set org directory
(setq org-directory "~/data/annex/wiki")
;; Set the agenda files
(setq org-agenda-files (list "~/wiki/index.org"
			     "~/wiki/projects/"
			     "~/wiki/support/"))
;; Display inline images
(setq org-startup-with-inline-images t)
;; Ask for confirmation before running babel, shell link or elisp link
(setq org-confirm-babel-evaluate t)
(setq org-confirm-elisp-link-function 'yes-or-no-p)
(setq org-confirm-shell-link-function 'yes-or-no-p)
;; Do not indent the content of header
(setq org-adapt-indentation nil)
;; Activate emacs lisp and hledger
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (hledger . t)
   (shell . t)))

(defun dwarfmaster/org/update-all-stats ()
  "Update all statistics in org buffer"
  (interactive)
  (org-update-statistics-cookies t))
(defun dwarfmaster/agenda/todos-only ()
  "Select only some todos in org agenda"
  (interactive)
  (org-tags-view 'TODO-ONLY))

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
  

;; TODO setup LaTeX preview
;; Enable the org table editor mode on other modes by calling 'turn-on-orgtbl when
;; loading other modes. Can be automated with
;;    (add-hook 'message-mode-hook 'turn-on-orgtbl)

;; Org Agenda
;;   ___               _                    _      
;;  / _ \ _ _ __ _    /_\  __ _ ___ _ _  __| |__ _ 
;; | (_) | '_/ _` |  / _ \/ _` / -_) ' \/ _` / _` |
;;  \___/|_| \__, | /_/ \_\__, \___|_||_\__,_\__,_|
;;           |___/        |___/                    
;; When opening agenda, maximize it
(setq org-agenda-window-setup 'only-window)
;; Restore windows after leaving the agenda view
(setq org-agenda-restore-windows-after-quit t)
;; Start agenda view on sunday
(setq org-agenda-start-on-weekday 0)
;; Span of the agenda view
(setq org-agenda-span 'week)
;; Set the sorting algorithm in agenda views
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-up)
	(todo priority-down category-up)
	(tags priority-down category-up)
	(search category-up)))
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
;; TODO configure org-agenda-prefix-format
;; Enforce dependencies between tasks
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
;; Don't dim blocked tasks in general
(setq org-agenda-dim-blocked-tasks 'invisible)

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

(defun dwarfmaster/agenda/default ()
  "Open the default weekly agenda view"
  (interactive)
  (org-agenda nil "d"))
(defun dwarfmaster/agenda/review ()
  "Open the agenda view with things to review"
  (interactive)
  (org-agenda nil "i"))
(defun dwarfmaster/agenda/projects ()
  "Open the agenda view with project overview"
  (interactive)
  (org-agenda nil "p"))
(defun dwarfmaster/agenda/mobile ()
  "Open the agenda view with mobile headers"
  (interactive)
  (org-agenda nil "M"))


;; Org IDs
;;   ___             ___ ___  
;;  / _ \ _ _ __ _  |_ _|   \ 
;; | (_) | '_/ _` |  | || |) |
;;  \___/|_| \__, | |___|___/ 
;;           |___/            
;; Use IDs when storing a link current entry
(setq org-id-link-to-org-use-id t)
;; Update ID file
(org-id-update-id-locations)



;; Org Attach
;;   ___               _  _   _           _    
;;  / _ \ _ _ __ _    /_\| |_| |_ __ _ __| |_  
;; | (_) | '_/ _` |  / _ \  _|  _/ _` / _| ' \ 
;;  \___/|_| \__, | /_/ \_\__|\__\__,_\__|_||_|
;;           |___/                             
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
(push '("att" . org-attach-expand-link) org-link-abbrev-alist)

(defun dwarfmaster/org/attach/commit-org-attachment (file)
  "git (annex) add file if it is a file, recurse into it if it is a directory"
  (if (file-directory-p file)
      (dwarfmaster/org/attach/commit-org-dir file)
    (let ((file-info (shell-command-to-string (concat "file -ihb " file))))
      (if (string-match-p "^text" file-info)
	  (shell-command (concat nix/git " add " file))
	(shell-command (concat nix/git " annex add " file))))))

(defun dwarfmaster/org/attach/commit-org-dir (dir)
  "git (annex) add all files in directory"
  (cd dir)
  (mapcar 'dwarfmaster/org/attach/commit-org-attachment (directory-files dir t "^[^\\.]")))

(defun dwarfmaster/org/attach/commit-org-subtree ()
  "git (annex) add all attachements of the subtree at point"
  (let ((dir (org-attach-dir)))
    (when (and (not (null dir))
	       (file-directory-p dir))
      (dwarfmaster/org/attach/commit-org-dir dir))))

(defun dwarfmaster/org/attach/pre-commit (path)
  "git add or git annex add all attachements of the selected org file"
  (with-temp-buffer
    (find-file path)
    (org-map-entries 'dwarfmaster/org/attach/commit-org-subtree)))

;; LaTeX theorems
;;  _        _____   __  __  _____ _                             
;; | |   __ |_   _|__\ \/ / |_   _| |_  ___ ___ _ _ ___ _ __  ___
;; | |__/ _` || |/ -_)>  <    | | | ' \/ -_) _ \ '_/ -_) '  \(_-<
;; |____\__,_||_|\___/_/\_\   |_| |_||_\___\___/_| \___|_|_|_/__/
(defun dwarfmaster/org/insert-env (name)
  "Insert a LaTeX block with title"
  (forward-line 1)
  (insert (concat "#+ATTR_LATEX: :options ["
		  (read-string (concat name " name:"))
		  "]\n"))
  (insert (concat "#+BEGIN_" name "\n\n#+END_" name "\n")))
(defmacro dwarfmaster/org/make-insert-env (name)
  "Create a dwarfmaster/org/insert-,name interactive function"
  (let ((fname (concat "dwarfmaster/org/insert-" name)))
    `(defun ,(intern fname) ()
       ,(concat "Insert a " name " block")
       (interactive)
       (dwarfmaster/org/insert-env ,name))))
;; Define all the specific environments
(dwarfmaster/org/make-insert-env "theorem")
(dwarfmaster/org/make-insert-env "lemma")
(dwarfmaster/org/make-insert-env "prop")
(dwarfmaster/org/make-insert-env "definition")
(dwarfmaster/org/make-insert-env "property")
(dwarfmaster/org/make-insert-env "remark")


;; Org roam
;;   ___             ___                 
;;  / _ \ _ _ __ _  | _ \___  __ _ _ __  
;; | (_) | '_/ _` | |   / _ \/ _` | '  \ 
;;  \___/|_| \__, | |_|_\___/\__,_|_|_|_|
;;           |___/                       
;; Org roam needs access to sqlite3
(setq exec-path (append exec-path nix/sqlite3-bin-dir))
;; Use wiki as org roam main directory
(setq org-roam-directory "~/wiki")
;; Set the index file
(setq org-roam-index-file (concat org-roam-directory "/index.org"))
;; Start/update the org-roam database on init
(add-hook 'after-init-hook 'org-roam-mode)
;; Store the database in $XDG_CACHE_HOME/org-roam/db.sqlite3
(setq org-roam-db-location (concat (getenv "XDG_CACHE_HOME")
				   "/org-roam/db.sqlite3"))
;; Position of the backlinks buffer
(setq org-roam-buffer-position 'right)
;; Width of the buffer
(setq org-roam-buffer-width 80)
;; Use helm
(setq org-roam-completion-system 'helm)
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



;; Capture
;;   ___           _                
;;  / __|__ _ _ __| |_ _  _ _ _ ___ 
;; | (__/ _` | '_ \  _| || | '_/ -_)
;;  \___\__,_| .__/\__|\_,_|_| \___|
;;           |_|                    
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

(defmacro dwarfmaster/capture/make-function (key name)
  "Make a function for capture key"
  (let ((fname (intern (concat "dwarfmaster/capture/" key))))
    `(defun ,fname ()
       ,(concat "Launch capture " (downcase name))
       (interactive)
       (org-capture nil ,key)
       )))
(dwarfmaster/capture/make-function "i" "Idea")
(dwarfmaster/capture/make-function "t" "Todo")
(dwarfmaster/capture/make-function "P" "Problem")


;; Bibliography
;;  ___ _ _    _ _                         _        
;; | _ |_) |__| (_)___  __ _ _ _ __ _ _ __| |_ _  _ 
;; | _ \ | '_ \ | / _ \/ _` | '_/ _` | '_ \ ' \ || |
;; |___/_|_.__/_|_\___/\__, |_| \__,_| .__/_||_\_, |
;;                     |___/         |_|       |__/ 
;; Set the dialect to biblatex (biber ?)
(bibtex-set-dialect 'biblatex)
;; Default bibliography
(setq bibtex-completion-bibliography '("~/data/annex/references.bib"))
(setq org-ref-default-bibliography '("~/data/annex/references.bib"))
;; Where to store the downloaded pdfs
(setq bibtex-completion-library-path '("~/data/annex/papers"))
(setq org-ref-pdf-directory "~/data/annex/papers")
;; Bibtex key configuration
(setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
;; Use helm in org-ref
(setq org-ref-completion-library 'org-ref-helm-bibtex)
;; Symbol indicating that the pdf is present
(setq bibtex-completion-pdf-symbol "")
;; Symbol indication that a note is present
(setq bibtex-completion-notes-symbol "")
;; Open PDFs with zathura
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
	(call-process nix/zathura nil 0 nil fpath)))
;; Handle pdf, djvu and epub 
(setq bibtex-completion-pdf-extension '("pdf" "djvu" "epub"))
;; Use firefox for opening links
(setq bibtex-completion-browser-function
      (lambda (url _) (start-process "firefox" "firefox*" nix/firefox url)))
;; Template used by org-roam-bibtex
(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
	 :file-name "refs/${citekey}"
	 :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+TAGS: ${keywords}\n"
	 :unnarrowed t)))
;; Open pdfs with zathura
(setq helm-bibtex-pdf-open-function
      (lambda (fpath)
	(start-process "zathura" "*helm-bibtex-zathura*" nix/zathura fpath))) 


;; NotDeft, for quick navigation
;;  _  _     _   ___       __ _   
;; | \| |___| |_|   \ ___ / _| |_ 
;; | .` / _ \  _| |) / -_)  _|  _|
;; |_|\_\___/\__|___/\___|_|  \__|
;; Set NotDeft directory
(setq notdeft-directories '("~/wiki"))
;; Extension of file notdeft is looking for
(setq notdeft-extension "org")

;; TODO when notdeft is available on nixos
;; (require 'notdeft)


;; Refiling
;;  ___      __ _ _ _           
;; | _ \___ / _(_) (_)_ _  __ _ 
;; |   / -_)  _| | | | ' \/ _` |
;; |_|_\___|_| |_|_|_|_||_\__, |
;;                        |___/ 
;; Refile to currently clocked item
(defun dwarfmaster/org/refile-to-clock ()
  "Refile to currently clocked task"
  (interactive)
  (org-refile 2))
;; Clear the refile cache
(defun dwarfmaster/org/refile-cache-clear ()
  "Clear the refile cache"
  (interactive)
  (org-refile-cache-clear))

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

;; Syncing
;;  ___              _           
;; / __|_  _ _ _  __(_)_ _  __ _ 
;; \__ \ || | ' \/ _| | ' \/ _` |
;; |___/\_, |_||_\__|_|_||_\__, |
;;      |__/               |___/ 

(defconst *org-sync-remote* "/scp:org@dwarfmaster.net:/srv/http/org/mobile.org")
(defconst *org-sync-temp*   "/tmp/luc/mobile.org")
(defconst *org-sync-dir*    "/data/luc/annex/")
(defconst *org-sync-tag*    "mobile")

(defun dwarfmaster/org/sync/kill-temp-buffer ()
  "Kill buffer opended on temp file if it exists"
  (let ((buf (get-file-buffer *org-sync-temp*)))
    (when (not (null buf)) (kill-buffer buf))))

;; Pushing
;;  , _                            
;; /|/ \       ,  |)   o        _, 
;;  |__/|  |  / \_|/\  | /|/|  / | 
;;  |    \/|_/ \/ |  |/|/ | |_/\/|/
;;                              (| 

(defun dwarfmaster/org/sync/push/process-subtree (tag file)
  "If the subtree at point has tag, then copy it to file"
  (let ((tags (org-get-tags)))
    (when (member tag tags)
      (org-id-get-create)
      (org-copy-subtree)
      (with-temp-buffer
	(find-file file)
	(goto-char (point-max))
	(org-paste-subtree 1)
	(save-buffer)))))

(defun dwarfmaster/org/sync/push/collates-all-with-tag (tag file)
  "Copy all subtress in the current buffer with a specific tag to file"
  (org-map-entries (lambda () (dwarfmaster/org/sync/push/process-subtree tag file))
		   nil 'agenda))

(defun dwarfmaster/org/sync/push/do ()
  "Synchronise subtress to phone"
  (interactive)
  (with-temp-buffer
    (delete-file *org-sync-temp*)
    (dwarfmaster/org/sync/push/collates-all-with-tag *org-sync-tag* *org-sync-temp*)
    (copy-file *org-sync-temp* *org-sync-remote* t)
    (dwarfmaster/org/sync/kill-temp-buffer)
    (delete-file *org-sync-temp*)))


;; Pulling
;;  , _                         
;; /|/ \      |\ |\ o        _, 
;;  |__/|  |  |/ |/ | /|/|  / | 
;;  |    \/|_/|_/|_/|/ | |_/\/|/
;;                           (| 

(defun dwarfmaster/org/sync/pull/dispatch-to-inbox ()
  "Dispatch current subtree to inbox"
  (org-copy-subtree)
  (message "Dispatching \"%s\" to inbox !\n" (thing-at-point 'line t))
  (with-temp-buffer
    (find-file org-default-notes-file)
    (goto-char (point-min))
    (re-search-forward "^* Mobile")
    (move-end-of-line nil)
    (open-line 1)
    (next-line 1)
    (org-paste-subtree 2)
    (save-buffer)))

(defun dwarfmaster/org/sync/pull/dispatch-to-id (id)
  "Replace subtree with id by the subtree at point"
  (let ((ntree (org-id-find id))
	(lvl   0))
    (if (null ntree) (dwarfmaster/org/sync/pull/dispatch-to-inbox)
      (org-copy-subtree)
      (message "Dispatching \"%s\" to old position !\n" (thing-at-point 'line t))
      (with-temp-buffer
	(find-file (car ntree))
	(goto-char (cdr ntree))
	(setq lvl (org-outline-level))
	(org-mark-subtree)
	(kill-region (region-beginning) (region-end))
	(current-kill 1)
	(org-paste-subtree lvl)
	(save-buffer)))))

(defun dwarfmaster/org/sync/pull/dispatch-subtree ()
  "Dispatch the subtree at point to the correct place"
  (let ((id (org-entry-get (point) "ID")))
    (message "Dispatching entry with id : %s\n" id)
    (if (null id)
	(dwarfmaster/org/sync/pull/dispatch-to-inbox)
      (dwarfmaster/org/sync/pull/dispatch-to-id id))))

(defun dwarfmaster/org/sync/pull/dispatch-file (file)
  "Dispatch all subtress in file with IDs to the relevant place"
  (with-temp-buffer
    (find-file file)
    (org-map-entries 'dwarfmaster/org/sync/pull/dispatch-subtree "LEVEL=1")))

(defun dwarfmaster/org/sync/pull/do ()
  "Synchronize subtrees from phone"
  (interactive)
  (let ((local  *org-sync-temp*)
	(remote *org-sync-remote*)
	(mgit   *org-sync-dir*))
    (with-temp-buffer
      (cd mgit)
      (when (or (not (null (magit-unstaged-files)))
		(not (null (magit-staged-files))))
	(magit-stash-both "Saving for mobile synchronisation"))
      (copy-file remote local t)
      (dwarfmaster/org/sync/pull/dispatch-file local)
      (dwarfmaster/org/sync/kill-temp-buffer)
      (delete-file local))))

;; All requires for org
;;  ___               _            
;; | _ \___ __ _ _  _(_)_ _ ___ ___
;; |   / -_) _` | || | | '_/ -_|_-<
;; |_|_\___\__, |\_,_|_|_| \___/__/
;;            |_|                  
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-protocol)
(require 'org-capture)

(require 'helm-org)
(require 'org-roam)
(require 'company-org-roam)
(push 'company-org-roam company-backends)

(require 'org-ref)
(require 'doi-utils)
(require 'org-ref-isbn)
(require 'org-ref-arxiv)
(require 'org-roam-bibtex)
(require 'helm-bibtex)

(require 'orgit)

;;; RSS
;;  ___  ___ ___ 
;; | _ \/ __/ __|
;; |   /\__ \__ \
;; |_|_\|___/___/

(require 'elfeed)
(setq elfeed-curl-program-name nix/curl)

(require 'elfeed-goodies)
(elfeed-goodies/setup)

(require 'elfeed-org)
;; Start elfeed-org
(elfeed-org)
;; Set file to look for feeds in
(setq rmh-elfeed-org-files (list "~/wiki/support/feeds.org"))
              


;;; Interface
;;  ___       _             __                
;; |_ _|_ __ | |_ ___ _ __ / _| __ _  ___ ___ 
;;  | || '_ \| __/ _ \ '__| |_ / _` |/ __/ _ \
;;  | || | | | ||  __/ |  |  _| (_| | (_|  __/
;; |___|_| |_|\__\___|_|  |_|  \__,_|\___\___|
;;                                            

;; Set the cursor color based on the evil state
(setq evil-emacs-state-cursor   `(,(plist-get dwarfmaster/colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get dwarfmaster/colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get dwarfmaster/colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get dwarfmaster/colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get dwarfmaster/colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get dwarfmaster/colors :base09) box))

;; Configure the GUI
(if window-system
  (progn (scroll-bar-mode -1) ; Hide the scroll bar
         (menu-bar-mode -1)   ; Hide the menu bar
         (tool-bar-mode -1)   ; Hide the toolbar
  )
)

;; Set the font
(add-to-list 'default-frame-alist
             '(font . "FuraCode Nerd Font Mono-11:weight=bold"))

;; Miscellaneous interface
(global-font-lock-mode 1)          ; Syntax highlighting
(global-display-line-numbers-mode) ; Line numbers globally, require emacs >= 26
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)          ; Scroll line by line
(setq smooth-scroll-margin 5)      ; Keep 5 visible line around cursor when scrolling


;;; Language support
;;  _                                                  
;; | |    __ _ _ __   __ _ _   _  __ _  __ _  ___  ___ 
;; | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \/ __|
;; | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/\__ \
;; |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___||___/
;;                   |___/             |___/           

;; Language server protocol (LSP) support
(require 'lsp-mode)
; Add lsp diagnostic statistics to modeline
(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))
; TODO bind LSP keys under <leader>m
; TODO install and configure lsp-ui

; Integration with company-mode
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Haskell
;  _  _         _       _ _ 
; | || |__ _ __| |_____| | |
; | __ / _` (_-< / / -_) | |
; |_||_\__,_/__/_\_\___|_|_|
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
; Can't get hie to work :'(
; lsp-haskell still gives some without it, so I keep it
; (setq lsp-haskell-process-path-hie nix/hie-wrapper)


;; Nix
;  _  _ _     
; | \| (_)_ __
; | .` | \ \ /
; |_|\_|_/_\_\
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
; Nix-mode and company-mode
(push 'nix-company company-backends)


;; Hledger
;  _  _ _           _              
; | || | |   ___ __| |__ _ ___ _ _ 
; | __ | |__/ -_) _` / _` / -_) '_|
; |_||_|____\___\__,_\__, \___|_|  
;                    |___/         
(require 'hledger-mode)
; TODO configure in details
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "/home/luc/finance/misc/accounting/2019.journal")
(push 'hledger-company company-backends)

;; Idris
;  ___    _     _    
; |_ _|__| |_ _(_)___
;  | |/ _` | '_| (_-<
; |___\__,_|_| |_/__/
(require 'idris-mode)
(require 'helm-idris)
(defvar dwarfmaster/idris-show-term-widgets nil)
(defun idris-toggle-term-widgets ()
  "Toogle show/hide active terms"
  (interactive)
  (setq dwarfmaster/idris-show-term-widgets (not idris/show-term-widgets))
  (if idris/show-term-widgets
    (idris-add-term-widgets)
    (idris-remove-term-widgets))
  )

;; Coq with proof-general
;   ___           
;  / __|___  __ _ 
; | (__/ _ \/ _` |
;  \___\___/\__, |
;              |_|
; How to configure all of it to work with org-mode :
;    https://chame.co/writeups/org_coq/post.html
(require 'proof-general)
(require 'company-coq)
(setq coq-prog-name nix/coqtop)
(add-hook 'coq-mode-hook #'company-coq-mode)

; Interface configuration
(setq proof-auto-raise-buffers       t       ; Let proof-general manage proof buffers
      proof-three-window-enable      t       ; Do not share message and goal buffer in same window
      proof-delete-empty-windows     nil     ; Keep empty windows (like goal window at end of proof)
      proof-three-window-mode-policy 'hybrid ; In three window mode, use two columns
      proof-toolbar-enable           nil     ; Show toolbar for proof buffers
      proof-follow-mode              'ignore ; Do not move point when interpreting/going back up
      proof-disappearing-proofs      t       ; Hide proofs body as they are completed
      )

; Use helm and company-coq to look for theorems
; Taken from https://chame.co/writeups/org_coq/post.html
(defun dwarfmaster/coq-man-candidates (cache)
  "Produce an alist mapping Coq abbreviations to refman anchors using CACHE."
  (mapcar (lambda (x) (cons x (company-coq-get-prop 'anchor x))) cache))

(defvar dwarfmaster/coq-man-actions '(("Lookup" . company-coq-doc-buffer-refman)))

(defun dwarfmaster/coq-man (name)
  "Helm completion for Coq refman documentation lookup. Default input is NAME."
  (interactive (list (thing-at-point 'word t)))
  (helm :sources (list (helm-build-sync-source "Coq Vernacular"
                         :candidates (lambda () (dwarfmaster/coq-man-candidates company-coq--refman-vernac-abbrevs-cache))
                         :action dwarfmaster/coq-man-actions)
                       (helm-build-sync-source "Coq Tactic"
                         :candidates (lambda () (dwarfmaster/coq-man-candidates company-coq--refman-tactic-abbrevs-cache))
                         :action dwarfmaster/coq-man-actions)
                       (helm-build-sync-source "Coq Scope"
                         :candidates (lambda () (dwarfmaster/coq-man-candidates company-coq--refman-scope-abbrevs-cache))
                         :action dwarfmaster/coq-man-actions)
                       (helm-build-sync-source "Coq Ltac"
                         :candidates (lambda () (dwarfmaster/coq-man-candidates company-coq--refman-ltac-abbrevs-cache))
                         :action dwarfmaster/coq-man-actions))
        :buffer "*helm tonic/coq-man*"
        :input name))

(defvar dwarfmaster/proof-body-toggled nil)
(defun dwarfmaster/toggle-all-proofs ()
  "Toggle visibility of all completed proof body at once"
  (interactive)
  (setq dwarfmaster/proof-body-toggled (not dwarfmaster/proof-body-toggled))
  (if dwarfmaster/proof-body-toggled
      (pg-show-all-proofs)
      (pg-hide-all-proofs)))
(defun dwarfmaster/coq/force-start ()
  "Start proof general in current buffer, closing any previously openend buffer"
  (interactive)
  (proof-toggle-active-scripting 1))

;; Keybindings
;  _  __          _     _           _ _                 
; | |/ /___ _   _| |__ (_)_ __   __| (_)_ __   __ _ ___ 
; | ' // _ \ | | | '_ \| | '_ \ / _` | | '_ \ / _` / __|
; | . \  __/ |_| | |_) | | | | | (_| | | | | | (_| \__ \
; |_|\_\___|\__, |_.__/|_|_| |_|\__,_|_|_| |_|\__, |___/
;           |___/                             |___/     
(require 'hydra)

(general-create-definer leader-def
  ;; :prefix leader
  :prefix "SPC")

(leader-def
  :states 'normal
  "b" 'helm-mini              ; List buffers and recent files
  "F" 'helm-find-files        ; Find files
  "f" 'helm-projectile        ; Find file/buffer in current project, or switch project
 )
(leader-def
  :keymaps 'elfeed-search-mode
  "b" 'helm-mini
  )




;; Language leader
;  _                                      
; | |   __ _ _ _  __ _ _  _ __ _ __ _ ___ 
; | |__/ _` | ' \/ _` | || / _` / _` / -_)
; |____\__,_|_||_\__, |\_,_\__,_\__, \___|
;                |___/          |___/     
; Language binding for org
(defhydra dwarfmaster/hydra/language/org (:color teal :hint nil)
  "
Org mode - babel

^Exec^                 ^Navigation
^^^^---------------------------------------
[_r_] Block            [_n_] Next block
[_R_] Buffer           [_p_] Previous block
[_t_] Block            [_g_] Go to result
[_T_] Buffer           [_z_] Expand block
^ ^                    [_E_] Open in buffer
"
  ("r"    org-babel-execute-src-block)              ; Execute block at point
  ("R"    org-babel-execute-buffer)                 ; Execute all blocks in buffer
  ("t"    org-babel-tangle)                         ; Tangle block at point
  ("T"    org-babel-tangle-file)                    ; Tangle buffer
  ("n"    org-babel-next-src-block :color red)      ; Move to next block
  ("p"    org-babel-previous-src-block :color red)  ; Move to previous src block
  ("g"    org-babel-goto-src-block-result)          ; Go to block result
  ("z"    org-babel-expand-src-block)               ; Unfold src block
  ("E"    org-edit-special)                         ; Edit the source code example at point in its native mode
  ("<escape>"  nil :color blue)
  )
(leader-def
  :states '(normal visual)
  :keymaps 'org-mode-map
  "i"  'dwarfmaster/hydra/language/org/body)
;; Nix
(defhydra dwarfmaster/hydra/language/nix (:color blue :hint nil)
  "
Nix

^REPL
^^--------
[_r_] REPL
"
  ("r" nix-repl) ; TODO open repl in new window
  )
(leader-def
  :states 'normal
  :keymaps 'nix-mode-map
  "i"  'dwarfmaster/hydra/language/nix/body)
;; HLedger
(defhydra dwarfmaster/hydra/language/hledger (:color blue :hint nil)
  "
HLedger

^REPL
^^---------------
[_r_] Run command
"
  ("r" hledger-run-command)
  )
(leader-def
  :states 'normal
  :keymaps 'hledger-mode-map
  "i" 'dwarfmaster/hydra/language/hledger/body)
;; Idris
(defhydra dwarfmaster/hydra/language/idris (:color teal :hint nil)
  "
Idris

^REPL^                 ^Edit^             ^Analyze
^^^^^^-------------------------------------------------
[_r_] Interpret file   [_M_] Add clause   [_h_] Helm
[_R_] Focus REPL       [_m_] Case split   [_t_] Type
^ ^                    [_N_] Normalize    [_A_] Widgets
"
  ("h" helm-idris)                 ; Load a helm based idris doc looker
  ("r" idris-load-file)            ; Interpret current file in buffer
  ("R" idris-pop-to-repl)          ; Pop up the clause window
  ("M" idris-add-clause)           ; Add missing pattern match clause
  ("m" idris-case-split)           ; Case split variable under cursor
  ("t" idris-type-at-point)        ; Get the type of the expession under point
  ("A" idris-toggle-term-widgets)  ; Show active terms
  ("N" idris-normalize-term)       ; Normalize term at point
  ("<escape>"  nil :color blue)
  )
(leader-def
  :states 'normal
  :keymaps 'idris-mode-map
  "i" 'dwarfmaster/hydra/language/idris
  )
;; Coq
(defhydra dwarfmaster/hydra/language/coq (:color amaranth :hint nil)
  "
Coq - Proof General

^REPL^                     ^Proof^                 ^Doc^                  ^Navigation
^^^^^^^^-----------------------------------------------------------------------------------------
[_r_] Start PG             [_j_] Assert next       [_h_] Lookup symbols   [_h_] Start of cmd
[_R_] Reset layout         [_k_] Assert prev       ^ ^                    [_l_] End of cmd
[_n_] Rotate buffers       [_J_] Assert to point   ^ ^                    [_e_] End of locked
^ ^                        [_K_] Undo and delete   ^ ^                    [_v_] Toggle proof
^ ^                        ^ ^                     ^ ^                    [_V_] Toggle all proofs
"
  ("R" proof-layout-windows)                  ; Reset window layout
  ("e" proof-goto-end-of-locked)              ; Move point to end of prooved region
  ("r" dwarfmaster/coq/force-start)           ; Start PG on current buffer (and stop on any previous buffer it was on)
  ("n" proof-display-some-buffers)            ; Rotate through buffers
  ("h" proof-goto-command-start)              ; Move to beggining of command at point
  ("l" proof-goto-command-end)                ; Move to end of command at point
  ("j" proof-assert-next-command-interactive) ; Interpret next command
  ("k" proof-undo-last-successful-command)    ; Undo last command
  ("J" proof-goto-point)                      ; Interpret all command up to point, or retract to point
  ("K" proof-undo-and-delete-last-successful-command)
  ("h" dwarfmaster/coq-man :color blue)       ; Lookup for symbol/theorem with helm
  ("v" pg-toggle-visibility)                  ; Toggle visibility of proof body under point
  ("V" dwarfmaster/toggle-all-proofs)         ; Toggle visibility of all completed proofs body at once
  ("<escape>"  nil :color blue)
  )
(leader-def
  :states 'normal
  :keymaps 'coq-mode-map
  "i" 'dwarfmaster/hydra/language/coq/body
  )
(defhydra dwarfmaster/hydra/language/bib (:color blue :hint nil)
  "
BibTex

^Import^               ^Navigation^        ^Misc
^^^^^^--------------------------------------------------------------
[_d_] DOI              [_j_] Next entry    [_o_] Misc
[_q_] Crossref query   [_k_] Prev entry    [_i_] New entry
[_b_] ISBN             ^ ^                 [_f_] Act on file
[_x_] Arxiv            ^ ^                 [_X_] Download from Arxiv
"
  ("d"  doi-utils-add-bibtex-entry-from-doi)
  ("q"  doi-utils-add-entry-from-crossref-query)
  ("j"  org-ref-bibtex-next-entry :color red)
  ("k"  org-ref-bibtex-prev-entry :color red)
  ("o"  org-ref-bibtex-hydra/body)
  ("i"  org-ref-bibtex-new-entry/body)
  ("f"  org-ref-bibtex-file/body)
  ("b"  isbn-to-bibtex)
  ("x"  arxiv-add-bibtex-entry)
  ("X"  arxiv-get-pdf)
  )
(leader-def
  :states 'normal
  :keymaps 'bibtex-mode-map
  "i" 'dwarfmaster/hydra/language/bib/body
  )
;; Elfeed
(defhydra dwarfmaster/hydra/language/elfeed (:color blue :hint nil)
  "
Elfeed RSS Reader

^Item^                 ^Search^         ^Misc^
^^^^^-----------------
[_i_] Show             [_s_] Search     [_u_] Refresh
[_t_] Tag              [_c_] Clear      [_U_] Fetch
[_T_] Untag            ^ ^              [_q_] Quit
[_r_] Mark as read
[_R_] Mark as unread

"
  ("i" elfeed-search-show-entry)
  ("t" elfeed-search-tag-all)
  ("T" elfeed-search-untag-all)
  ("r" elfeed-search-untag-all-unread)
  ("R" elfeed-search-tag-all-unread)
  ("s" elfeed-search-live-filter)
  ("c" elfeed-search-clear-filter)
  ("u" elfeed-search-update--force)
  ("U" elfeed-search-fetch)
  ("q" elfeed-search-quit-window)
  )
(leader-def
  :states 'normal
  :keymaps 'elfeed-search-mode-map
  "i" 'dwarfmaster/hydra/language/elfeed/body)



;; Miscellaneous
;  __  __ _            _ _                           
; |  \/  (_)___ __ ___| | |__ _ _ _  ___ ___ _  _ ___
; | |\/| | (_-</ _/ -_) | / _` | ' \/ -_) _ \ || (_-<
; |_|  |_|_/__/\__\___|_|_\__,_|_||_\___\___/\_,_/__/
(defhydra dwarfmaster/hydra/misc (:color blue :hint nil)
  "
Miscellaneous

^Org^              ^Doc^              ^Misc^                   ^Programs
^^^^^^^--------------------------------------------------------------------
[_l_] Store link   [_s_] Symbol         [_h_] All commands     [_e_] Elfeed
[_c_] Capture      [_v_] Variable       [_R_] Select color     [_C_] Calcul
^ ^                [_b_] Bindings       [_f_] Figlet
^ ^                [_h_] All commands   [_F_] Figlet small
"
  ("f" dwarfmaster/make-figlet-text-normal)
  ("F" dwarfmaster/make-figlet-text-small)
  ("h" helm-M-x)               ; Display all emacs commands with keys
  ("R" helm-color)             ; Select and previous colors
  ("C" helm-calcul-expression) ; Helm interface to the calc command
  ("l" org-store-link)
  ("c" dwarfmaster/hydra/capture/body)
  ("s" describe-symbol)
  ("v" describe-variable)
  ("b" describe-bindings)
  ("e" elfeed)
  )
(leader-def
  :states 'normal
  "w"  'dwarfmaster/hydra/misc/body
  )

;; Capture sub-hydra
;;   ___           _                
;;  / __|__ _ _ __| |_ _  _ _ _ ___ 
;; | (__/ _` | '_ \  _| || | '_/ -_)
;;  \___\__,_| .__/\__|\_,_|_| \___|
;;           |_|                    
(defhydra dwarfmaster/hydra/capture (:color blue :hint nil)
  "
Capture

^Tasks
^^-----------
[_i_] Idea
[_t_] Todo
[_p_] Problem
"
  ("i"  dwarfmaster/capture/i)
  ("t"  dwarfmaster/capture/t)
  ("p"  dwarfmaster/capture/P)
  )

;; Helm
;  _  _     _       
; | || |___| |_ __  
; | __ / -_) | '  \ 
; |_||_\___|_|_|_|_|
(defhydra dwarfmaster/hydra/helm (:color blue :hint nil)
  "
Helm

^Interaction
^^----------
[_r_] Resume
"
  ("r" helm-resume) ; Resume the previous helm with the same thing typed
  )
(leader-def
  :states 'normal
  "h" 'dwarfmaster/hydra/helm/body
  )

;; Project
;  ___          _        _   
; | _ \_ _ ___ (_)___ __| |_ 
; |  _/ '_/ _ \| / -_) _|  _|
; |_| |_| \___// \___\__|\__|
;            |__/            
(defhydra dwarfmaster/hydra/project (:color blue :hint nil)
  "
Projects

^Project^                  ^Misc
^^^^----------------------------
[_f_] Find file            [_p_] Switch project
[_d_] Find dir             [_r_] Recent files
[_s_] Other file           [_R_] Invalidate cache
[_g_] Find file at point
"
 ("f" helm-projectile-find-file)       ; Find file in project
 ("d" helm-projectile-find-dir)        ; Find directory in project
 ("r" helm-projectile-recentf)         ; Find recently opened files
 ("s" helm-projectile-find-other-file) ; Jump to an other file with same name but different extension
 ("R" projectile-invalidate-cache)     ; Recache project files
 ("p" helm-projectile-switch-project)  ; Switch project
 ("g" helm-projectile-find-file-dwim)  ; Find file at point
 )
(leader-def
 :states '(normal visual)
 "p"  'dwarfmaster/hydra/project/body
 )

;; Magit
;  __  __           _ _   
; |  \/  |__ _ __ _(_) |_ 
; | |\/| / _` / _` | |  _|
; |_|  |_\__,_\__, |_|\__|
;             |___/       
(general-create-definer vcs-leader-def
  :prefix "SPC v")
;; Not using hydra since there already is the ? key to display a menu
; Magit binding valid in non-magit buffers
(dwarfmaster/unbind-normal-keys 'magit-file-mode-map)
(vcs-leader-def
 :states '(normal visual)
 :keymaps 'magit-file-mode-map
 "S" 'magit-status
 "?" 'magit-file-dispatch
 "s" 'magit-stage-file
 "u" 'magit-unstage-file
 "c" 'magit-commit
 "D" 'magit-diff
 "d" 'magit-diff-buffer-file
 "L" 'magit-log
 "l" 'magit-log-buffer-file
 "t" 'magit-log-trace-definition
 "B" 'magit-blame
 "e" 'magit-edit-line-commit
 "p" 'magit-blob-previous
 "C" 'magit-file-checkout
 "W" 'magit-wip-commit
 "w" 'magit-wip-log-current
 )

; Main magit mode bindings
(dwarfmaster/unbind-normal-keys 'magit-mode-map)
(dwarfmaster/unbind-normal-keys 'magit-status-mode-map)
(general-define-key
 :states 'normal
 :keymaps  'magit-mode-map
 "S-SPC"   'magit-diff-show-or-scroll-up
 "TAB"     'magit-section-toggle
 "RET"     'magit-visit-thing
 "C-TAB"   'magit-section-cycle
 "DEL"     'magit-section-cycle-global
 )
(vcs-leader-def
  :states '(normal visual)
  :keymaps 'magit-status-mode-map
  "TAB" 'magit-section-toggle
  "RET" 'magit-visit-thing
  "!"   'magit-run
  "$"   'magit-process-buffer
  "%"   'magit-worktree
  "+"   'magit-diff-more-context
  "-"   'magit-diff-less-context
  "0"   'magit-diff-default-context
  "1"   'magit-section-show-level-1
  "2"   'magit-section-show-level-2
  "3"   'magit-section-show-level-3
  "4"   'magit-section-show-level-4
  "5"   'digit-argument
  "6"   'digit-argument
  "7"   'digit-argument
  "8"   'digit-argument
  "9"   'digit-argument
  ":"   'magit-git-command
  "<"   'beginning-of-buffer
  ">"   'end-of-buffer
  "?"   'magit-dispatch
  "A"   'magit-cherry-pick
  "B"   'magit-bisect
  "C"   'magit-clone
  "D"   'magit-diff-refresh
  "E"   'magit-ediff
  "F"   'magit-pull
  "G"   'magit-refresh-all
  "I"   'magit-gitignore
  "K"   'magit-file-untrack
  "L"   'magit-log-refresh
  "M"   'magit-remote
  "O"   'magit-subtree
  "P"   'magit-push
  "R"   'magit-file-rename
  "S"   'magit-stage-modified
  "T"   'magit-notes
  "U"   'magit-unstage-all
  "V"   'magit-revert
  "W"   'magit-patch
  "X"   'magit-reset
  "Y"   'magit-cherry
  "Z"   'magit-stash
  "^"   'magit-section-up
  "b"   'magit-branch
  "c"   'magit-commit
  "d"   'magit-diff
  "e"   'magit-ediff-dwim
  "f"   'magit-fetch
  "g"   'magit-refresh
  "h"   'magit-dispatch
  "i"   'magit-gitignore
  "j"   'magit-status-jump
  "k"   'magit-delete-thing
  "l"   'magit-log
  "m"   'magit-merge
  "n"   'magit-section-forward
  "o"   'magit-submodule
  "p"   'magit-section-backward
  "q"   'magit-mode-bury-buffer
  "r"   'magit-rebase
  "s"   'magit-stage
  "t"   'magit-tag
  "v"   'magit-revert-no-commit
  "w"   'magit-am
  "x"   'magit-reset-quickly
  "y"   'magit-show-refs
  "z"   'magit-stash
  "DEL" 'magit-diff-show-or-scroll-down
  )

;; Org table
;   ___             _____     _    _     
;  / _ \ _ _ __ _  |_   _|_ _| |__| |___ 
; | (_) | '_/ _` |   | |/ _` | '_ \ / -_)
;  \___/|_| \__, |   |_|\__,_|_.__/_\___|
;           |___/                        
(defhydra dwarfmaster/hydra/org/table (:color amaranth :hint nil)
  "
^Navigation^                 ^Formatting^           ^Editing^            ^I/O^           ^Computation
^^^^^^^^^^-------------------------------------------------------------------------------------------
[_h_] Prev field             [_a_] Align            [_i_] Insert col     [_I_] Import    [_=_] Eval formula
[_j_] Next row               [_H_] Move col left    [_r_] Insert row     [_:_] Export    [_+_] Sum
[_l_] Next field             [_L_] Move col right   [_-_] Insert line    [_C_] Create    [_*_] Recalculate
[_^_] Field beg              [_J_] Move row down    [_d_] Kill row       ^ ^             [_#_] Iterate
[_$_] Field end              [_K_] Move row up      [_e_] Edit field     ^ ^             [_|_] Recalculate all
[_z_] Shring sized cols      [_S_] Sort lines       [_x_] Cut            ^ ^
[_Z_] Expand all cols        [_C-a_] Copy down      [_y_] Copy           ^ ^
[_TAB_] Toggle col width     ^ ^                    [_p_] Paste          ^ ^
"
 ("C"   org-table-create-or-convert-from-region)  ; Convert region to table
 ("a"   org-table-align)          ; Re-align table
 ("h"   org-table-previous-field)
 ("j"   org-table-next-row)
 ("l"   org-table-next-field)
 ("^"   org-table-beginning-of-field)
 ("$"   org-table-end-of-field)
 ("I"   org-table-import :color blue)          ; Import file as table
 (":"   org-table-export :color blue)          ; Export a table as file
 ("z"   org-table-shrink)          ; Shrink all columns with size
 ("Z"   org-table-expand)          ; Expand all columns
 ("TAB" org-table-toggle-column-width)
 ("}"   org-table-toggle-coordinate-overlays) ; Display an overlay with fields coordinates
 ("H"   org-table-move-column-left)
 ("J"   org-table-move-row-down)
 ("K"   org-table-move-row-up)
 ("L"   org-table-move-column-right)
 ("i"   org-table-insert-column)
 ("r"   org-table-insert-row)
 ("-"   org-table-insert-hline)
 ("d"   org-table-kill-row)
 ("S"   org-table-sort-lines)
 ("e"   org-table-edit-field :color blue)      ; Edit field in another buffer
 ("C-a" org-table-copy-down)       ; Copy current field down, incrementing integers
 ("="   org-table-eval-formula)    ; Insert formula for current field
 ("x"   org-table-cut-region)
 ("y"   org-table-copy-region)
 ("p"   org-table-paste-rectangle)
 ("+"   org-table-sum)         ; Sum all numbers in current region or column
 ("*"   org-table-recalculate) ; Recalculate current row
 ("#"   org-table-iterate)     ; Recalculate current table until it stabilies
 ("|"   org-table-recalculate-buffer-tables) ; Recalculate all tables in buffer
 ("<escape>"  nil :color blue)
 )


;; Org displacement
;   ___             ___  _         _                           _   
;  / _ \ _ _ __ _  |   \(_)____ __| |__ _ __ ___ _ __  ___ _ _| |_ 
; | (_) | '_/ _` | | |) | (_-< '_ \ / _` / _/ -_) '  \/ -_) ' \  _|
;  \___/|_| \__, | |___/|_/__/ .__/_\__,_\__\___|_|_|_\___|_||_\__|
;           |___/            |_|                                   
(defhydra dwarfmaster/hydra/org/displacement (:color amaranth :hint nil)
  "
Org displacement

^Subtrees^         ^Lists^           ^Refiling^        ^Misc
^^^^^^^^----------------------------------------------------
[_<_] Promote      [_j_] Move up     [_r_] Refile      [_S_] Sort
[_>_] Demote       [_k_] Move down   [_R_] Copy
[_J_] Move up      ^ ^               [_A_] Archive
[_K_] Move down
[_d_] Cut
[_y_] Copy
[_p_] Paste
"
 ("<"    org-promote-subtree)             ; Promote current subtree
 (">"    org-demote-subtree)              ; Demote current subtree
 ("J"    org-move-subtree-down)
 ("K"    org-move-subtree-up)
 ("d"    org-cut-subtree)                  ; Cut current subtree
 ("y"    org-copy-subtree)                 ; Copy current subtree
 ("p"    org-paste-subtree)                ; Cleverly paste current subtree, adapting level
 ("j"    org-move-item-down)               ; Move list item down
 ("k"    org-move-item-up)                 ; Move list item up
 ("A"    org-archive-subtree :color blue)  ; Archive subtree
 ("r"    org-refile :color blue)           ; Refile the entry or region at point
 ("R"    org-copy :color blue)             ; Like refiling, but without deleting
 ("S"    org-sort :color blue)             ; Sort same-level entries
 ("<escape>"  nil :color blue)
 ;; TODO find commands to shift indents in list item and bind them to h,l
 ;; TODO find commands to change item bullet and bind them to *
 )

;; Org Insert
;   ___             ___                  _   
;  / _ \ _ _ __ _  |_ _|_ _  ___ ___ _ _| |_ 
; | (_) | '_/ _` |  | || ' \(_-</ -_) '_|  _|
;  \___/|_| \__, | |___|_||_/__/\___|_|  \__|
;           |___/                            
(defhydra dwarfmaster/hydra/org/insert (:color blue :hint nil)
  "
Org insert

^Heading^                    ^Timestamp^            ^Misc
^^^^^^-----------------------------------------------------------------------
[_H_] At point               [_s_] Timestamp        [_RET_] Contextual insert
[_h_] After subtree          [_S_] Inactive         [_l_] Link
[_T_] TODO at point          [_d_] Schedule         [_r_] Link to roam note
[_t_] TODO after subtree     [_D_] Deadline         [_c_] Citation
^ ^                          ^ ^                    ^ ^
"
 ("H"    org-insert-heading)                      ; Insert heading at point
 ("h"    org-insert-heading-respect-content)      ; Insert heading after subtree
 ("T"    org-insert-todo-heading)                 ; Insert todo at point
 ("t"    org-insert-todo-heading-respect-content) ; Insert todo after subtree
 ("s"    org-time-stamp)                          ; Insert timestamp
 ("S"    org-time-stamp-inactive)                 ; Insert inactive timestamp
 ("d"    org-schedule)                            ; Insert schedule entry
 ("D"    org-deadline)                            ; Insert deadline entry
 ("RET"  org-meta-ret)                            ; Insert new heading/item/table row
 ("l"    org-insert-link)                         ; Insert link at current position (or over current text)
 ("r"    org-roam-insert)                         ; Insert link to roam note
 ("c"    helm-bibtex)                             ; Insert a citation from bibtex
 )

;; Org Math
;;   ___             __  __      _   _       
;;  / _ \ _ _ __ _  |  \/  |__ _| |_| |_  ___
;; | (_) | '_/ _` | | |\/| / _` |  _| ' \(_-<
;;  \___/|_| \__, | |_|  |_\__,_|\__|_||_/__/
;;           |___/                           
(defhydra dwarfmaster/hydra/org/maths (:color blue :hint nil)
  "
Org Mathematics

^Insertion^
^^----------------
[_t_] Theorem
[_l_] Lemma
[_p_] Property
[_P_] Proposition
[_r_] Remark
[_d_] Definition
"
  ("t"    dwarfmaster/org/insert-theorem)
  ("l"    dwarfmaster/org/insert-lemma)
  ("p"    dwarfmaster/org/insert-property)
  ("P"    dwarfmaster/org/insert-prop)
  ("r"    dwarfmaster/org/insert-remark)
  ("d"    dwarfmaster/org/insert-definition)
  )

;; Org Property
;   ___             ___                       _        
;  / _ \ _ _ __ _  | _ \_ _ ___ _ __  ___ _ _| |_ _  _ 
; | (_) | '_/ _` | |  _/ '_/ _ \ '_ \/ -_) '_|  _| || |
;  \___/|_| \__, | |_| |_| \___/ .__/\___|_|  \__|\_, |
;           |___/              |_|                |__/ 
(defhydra dwarfmaster/hydra/org/property (:color blue :hint nil)
  "
Org property

^Edit^            ^Execute^            ^Insertion^                ^Columns
^^^^^^^^-----------------------------------------------------------------------------
[_s_] Set         [_a_] Do action      [_i_] Insert drawer        [_C_] Start view
[_h_] Prev value  [_c_] Compute        [_d_] Delete               [_b_] Insert dblock
[_l_] Next value  ^ ^                  [_D_] Delete everywhere
"
 ("s"    org-set-property)               ; Set a property
 ("i"    org-insert-drawer)              ; Insert a property drawer for current entry
 ("a"    org-property-action)            ; Execute property command
 ("c"    org-compute-property-at-point)  ; Compute property at point
 ("h"    org-property-previous-allowed-value)
 ("l"    org-property-next-allowed-value)
 ("d"    org-delete-property)            ; Remove a property from current entry
 ("D"    org-delete-property-globally)   ; Remove a property from all entries in current file
 ("C"    org-columns)                    ; Start column view
 ("b"    org-insert-columns-dblock)      ; Insert a column dynamic block
 )

;; TODOS
;;  _____ ___  ___   ___  ___ 
;; |_   _/ _ \|   \ / _ \/ __|
;;   | || (_) | |) | (_) \__ \
;;   |_| \___/|___/ \___/|___/

;; Tasks and projects
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

;; TODO
(defun dwarfmaster/org/todo-switch-dwim ()
  "Open the right hydra depending on the sequence at point"
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

;; Org Attach
;;   ___               _  _   _           _    
;;  / _ \ _ _ __ _    /_\| |_| |_ __ _ __| |_  
;; | (_) | '_/ _` |  / _ \  _|  _/ _` / _| ' \ 
;;  \___/|_| \__, | /_/ \_\__|\__\__,_\__|_||_|
;;           |___/                             
(defhydra dwarfmaster/hydra/org/attach (:color blue :hint nil)
  "
Org Attach

^Attach^      ^Open^            ^Misc
^^^^^^-----------------------------------------
[_a_] Mv      [_o_] One         [_z_] Sync
[_c_] Cp      [_d_] Directory   [_s_] Set dir
[_D_] Delete  ^ ^               [_S_] Unset dir
"
  ("a"   org-attach-attach-mv)
  ("c"   org-attach-attach-cp)
  ("D"   org-attach-delete-one)
  ("o"   org-attach-open)
  ("d"   org-attach-reveal)
  ("z"   org-attach-sync)
  ("s"   org-attach-set-directory)
  ("S"   org-attach-unset-directory)
  
  ("<escape>"  nil :color blue)
  )

;; Org mode
;;   ___             __  __         _
;;  / _ \ _ _ __ _  |  \/  |___  __| |___
;; | (_) | '_/ _` | | |\/| / _ \/ _` / -_)
;;  \___/|_| \__, | |_|  |_\___/\__,_\___|
;;           |___/                        
;; Generic bindings, not in hydra
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "RET"  'org-open-at-point
 "TAB"  'org-cycle
 "DEL"  'org-mark-ring-goto  ; Jump to previous org mark
 )

(defhydra dwarfmaster/hydra/org-mode (:color blue :hint nil)
  "
Org mode

^Visibility^             ^TODOs^                  ^Navigation^                  ^Update^             ^Misc
^^^^^^^^^^------------------------------------------------------------------------------------------------------------------
[_z_] Reveal at point    [_!_] Toggle             [_h_] Heading up              [_+_] Stats          [_@_] Mark subtree
[_Z_] Reset              [_$_] Sparse view        [_j_] Next heading            [_#_] All stats      [_'_] Set tags
[_U_] Unfold all         [_c_] Toggle checkbox    [_k_] Prev heading            [_u_] DBlock         [_,_] Toggle pretty
[_O_] Tree in buffer     [_p_] Set priority       [_s_] Select heading          [_U_] All dblocks    [_*_] Toggle heading
[_/_] Sparse tree        [_e_] Set effort         [_]_] Next sparse match       [_R_] Refile cache   [_t_] Table mode
[_B_] Roam backlinks     ^ ^                      [_[_] Prev sparse match       [_D_] Roam DB        [_m_] Displacement mode
^ ^                      ^ ^                      [_%_] Push pos to org ring    ^ ^                  [_P_] Properties mode 
^ ^                      ^ ^                      [_o_] Open roam note          ^ ^                  [_i_] Insert mode
^ ^                      ^ ^                      [_b_] Bibtex link menu        ^ ^                  [_M_] Maths mode
^ ^                      ^ ^                      ^ ^                           ^ ^                  [_a_] Attach
"
 ;; Visibility
 ("Z"   org-set-startup-visibility)               ; Reset visibility to start
 ("z"   org-reveal)                               ; Reveal context around point  
 ("U"   outline-show-all)                         ; Unfold everything
 ("O"   org-tree-to-indirect-buffer :color blue)  ; Open current subtree in an indirect buffer
 ("/"   org-sparse-tree :color blue)              ; Create sparse tree for match
 ("B"   org-roam)                                 ; Toggle backlinks

 ;; TODOs
 ("!"   dwarfmaster/org/todo-switch-dwim :color blue)  ; Toggle the todo mark along a sequence
 ("$"   org-todo-list)                                 ; Display the window as sparse tree with
                                                       ; only TODO buffers
 ("c"   org-toggle-checkbox)                           ; Toggle checkbox state
 ("p"   org-priority :color blue)                      ; Set priority for current TODO item
 ("e"   org-set-effort :color blue)                    ; Set the effort estimate for a task

 ;; Navigation
 ("h"   outline-up-heading :color amaranth)           ; Move to the buffer above
 ("j"   org-next-visible-heading :color amaranth)     ; Move to next heading
 ("k"   org-previous-visible-heading :color amaranth) ; Move to previous heading
 ("s"   helm-org-in-buffer-heading :color blue)       ; Find heading in buffer
 ("]"   next-error :color amaranth)                   ; Jump to next sparse tree match
 ("["   previous-error :color amaranth)               ; Jump to previous sparse tree match
 ("%"   org-mark-ring-push :color amaranth)           ; Push current position in org mark ring
 ("o"   org-roam-find-file :color blue)               ; Open roam note
 ("b"   org-ref-bibtex-hydra/body :color blue)        ; Contextual menu on cite link

 ;; Dynamic
 ("+"   org-update-statistics-cookies)      ; Update statistics for current entry
 ("#"   dwarfmaster/org/update-all-stats)   ; Update statistics in the whole file
 ("u"   org-dblock-update)                  ; Update dblock at point
 ("U"   org-update-all-dblocks)             ; Update all dblocks in file
 ("R"   dwarfmaster/org/refile-cache-clear) ; Reset the refile destination cache
 ("D"   org-roam-db-build-cache)            ; Rebuild org roam cache

 ;; Misc
 ("@"   org-mark-subtree :color blue)                  ; Select current subtree
 ("'"   org-set-tags-command :color blue)              ; Set tags for current heading
 (","   org-toggle-pretty-entities)        ; Toggle the pretifying
 ("*"   org-toggle-heading)                ; Toggle heading on line (or selected lines)
 ("t"   dwarfmaster/hydra/org/table/body :color blue)
 ("m"   dwarfmaster/hydra/org/displacement/body :color blue)
 ("P"   dwarfmaster/hydra/org/property/body :color blue)
 ("i"   dwarfmaster/hydra/org/insert/body  :color blue)
 ("M"   dwarfmaster/hydra/org/maths/body   :color blue)
 ("a"   dwarfmaster/hydra/org/attach/body  :color blue)

 ("<escape>"  nil :color blue)
 )
(leader-def
 :states '(normal visual)
 :keymaps 'org-mode-map
 "o"  'dwarfmaster/hydra/org-mode/body
 )

;; Clock
;;   ___ _         _   
;;  / __| |___  __| |__
;; | (__| / _ \/ _| / /
;;  \___|_\___/\__|_\_\
(defhydra dwarfmaster/hydra/clock/org (:color blue :hint nil)
  "
Clocking time

^Clocking^                    ^Info^                          ^Timer
^^^^^^-------------------------------------------------------------------------------------
[_s_] Start on current        [_u_] Update range              [_t_] Print, start if stopped
[_S_] Start on last           [_j_] Shift timestamp up        [_T_] Start deacreasing
[_e_] Stop                    [_k_] Shift timestamp down      [_-_] Print in list item
[_C_] Cancel current          [_d_] Display clock overlay     [_;_] (Re)start
[_r_] Refile under clocked    ^ ^                             [_._] Stop
[_E_] Edit effort of current  ^ ^                             [_p_] Pause/continue
"
 ;; Clock
 ("s"    org-clock-in)                     ; Start the clock on the current item
 ("u"    org-evaluate-time-range)          ; Recompute the time interval after editing a timestamp
 ("j"    org-clock-timestamps-down)        ; Decrease both start and end of a CLOCK line by same duration
 ("k"    org-clock-timestamps-up)          ; Decrease both start and end of a CLOCK line by same duration
 ("C"    org-clock-cancel)                 ; Cancel current clock
 ("d"    org-clock-display)                ; Display overlay with clock information on header lines
 ("E"    org-clock-modify-effort-estimate) ; Modify the effort estimate of the clocked item
 ("r"    dwarfmaster/org/refile-to-clock)  ; Refile as child to the currently clocked task
 ("e"    org-clock-out)                    ; Stop the clock
 ("S"    org-clock-in-last)                ; Re-clock the last closed task

 ;; timer
 ("t"    org-timer)                        ; Print relative timer value, start it if it wasn't started
 ("T"    org-timer-set-timer)              ; Start a decreasing timer
 ("-"    org-timer-item)                   ; Insert list item with relative timer value
 (";"    org-timer-start)                  ; (Re)Start relative timer with value 0
 ("."    org-timer-stop)                   ; Stop relative timer
 ("p"    org-timer-pause-or-continue)      ; Pause/Continue relative timer
 )
(leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "c"   'dwarfmaster/hydra/clock/org/body
 )
(defhydra dwarfmaster/hydra/clock/main (:color blue :hint nil)
  "
Clocking time : [_e_] Stop clock    [_S_] Re-clock last
"
 ("e"    org-clock-out)       ; Stop the clock
 ("S"    org-clock-in-last)   ; Re-clock the last closed task
 )
(leader-def
 :states 'normal
 "c"  'dwarfmaster/hydra/clock/main/body
 )

;; Org Agenda
;;   ___               _                    _      
;;  / _ \ _ _ __ _    /_\  __ _ ___ _ _  __| |__ _ 
;; | (_) | '_/ _` |  / _ \/ _` / -_) ' \/ _` / _` |
;;  \___/|_| \__, | /_/ \_\__, \___|_||_\__,_\__,_|
;;           |___/        |___/                    
(defhydra dwarfmaster/hydra/agenda (:color blue :hint nil)
  "
Org Agenda

^Views^                    ^Querying^                    ^Time^          ^Syncing^
^^^^^^^^---------------------------------------------------------------------------
[_a_] Agenda               [_T_] Select some TODOs       [_Y_] Yearly    [_P_] Push
[_i_] To review            [_s_] Select headers by tag   [_M_] Monthly   [_L_] Pull
[_p_] Projects overview    [_S_] Generic search          [_W_] Weekly    [_M_] List
[_<_] Restrict to subtree
[_>_] Remove restriction
"
  ("a"     dwarfmaster/agenda/default)
  ("i"     dwarfmaster/agenda/review)
  ("p"     dwarfmaster/agenda/projects)
  ("T"     dwarfmaster/agenda/todos-only)     ; Select todos matching specific tags
  ("s"     org-tags-view)                     ; List all headers matching a tag
  ("S"     org-search-view)                   ; General text search ability for org mode entries
  ;; TODO bind p to the previous agenda file
  ("<"     org-agenda-set-restriction-lock)    ; Restrict agenda to current subtree
  (">"     org-agenda-remove-restriction-lock) ; Remove restriction lock
  ("Y"     org-agenda-year-view)
  ("M"     org-agenda-month-view)
  ("W"     org-agenda-week-view)
  ("P"     dwarfmaster/org/sync/push/do)
  ("L"     dwarfmaster/org/sync/pull/do)
  ("M"     dwarfmaster/agenda/mobile)
 )
(leader-def
 :states '(normal visual)
 "a"   'dwarfmaster/hydra/agenda/body
 )


(provide 'main)
