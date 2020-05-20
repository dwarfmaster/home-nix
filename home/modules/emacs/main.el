
;; TODO bind ESC to keyboard-quit in all buffers 

;; Small utility
(defun load-local-file (path)
  "Load file from path relative to current file"
  (load-file (expand-file-name path (file-name-directory load-file-name))))

;; Load nix constants
(load-local-file "./nixpaths.el")

;; Unbind normal keys
; TODO improve logic
(defun dwarfmaster/select-normal-and-unbind (map key value)
  "If key is alphanumeric, unbind it from map"
  (if (number-or-marker-p key)
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


;; Keys
(general-create-definer leader-def
  ;; :prefix leader
  :prefix "SPC")
(general-create-definer remap-key
  :prefix remap)
(general-create-definer language-leader-def
  ;; :prefix language-leader
  :prefix "SPC i")
(general-create-definer fancy-leader-def
  :prefix "SPC w")
(general-create-definer helm-leader-def
  :prefix "SPC h")
(general-create-definer project-leader-def
  :prefix "SPC p")
(general-create-definer vcs-leader-def
  :prefix "SPC v")
(general-create-definer org-leader-def
  :prefix "SPC o")
(general-create-definer org-table-leader-def
  :prefix "SPC o t")
(general-create-definer org-displace-leader-def
  :prefix "SPC o m")
(general-create-definer org-insert-leader-def
  :prefix "SPC o i")
(general-create-definer org-property-leader-def
  :prefix "SPC o p")
(general-create-definer clock-leader-def
  :prefix "SPC c")
(general-create-definer org-agenda-leader-def
  :prefix "SPC a")

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
(fancy-leader-def
  :states 'normal
  "f" 'dwarfmaster/make-figlet-text-normal
  "F" 'dwarfmaster/make-figlet-text-small
  )


;; General
;   ____                           _ 
;  / ___| ___ _ __   ___ _ __ __ _| |
; | |  _ / _ \ '_ \ / _ \ '__/ _` | |
; | |_| |  __/ | | |  __/ | | (_| | |
;  \____|\___|_| |_|\___|_|  \__,_|_|
;                                    

;; Helm, ie navigation
;  _  _     _       
; | || |___| |_ __  
; | __ / -_) | '  \ 
; |_||_\___|_|_|_|_|
; https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

(leader-def
  :states 'normal
  "b" 'helm-mini              ; List buffers and recent files
  "F" 'helm-find-files        ; Find files
  )
(fancy-leader-def
  :states 'normal
  "h" 'helm-M-x               ; Display all emacs commands with keys
  "R" 'helm-color             ; Select and previous colors
  "c" 'helm-calcul-expression ; Helm interface to the calc command
  )
(helm-leader-def
  :states 'normal
  "r" 'helm-resume ; Resume the previous helm with the same thing typed
  )

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

(leader-def
 :states 'normal
 "f" 'helm-projectile ; Find file/buffer in current project, or switch project
 )
(project-leader-def
 :states 'normal
 "f" 'helm-projectile-find-file       ; Find file in project
 "d" 'helm-projectile-find-dir        ; Find directory in project
 "r" 'helm-projectile-recentf         ; Find recently opened files
 "s" 'helm-projectile-find-other-file ; Jump to an other file with same name but different extension
 "R" 'projectile-invalidate-cache     ; Recache project files
 "p" 'helm-projectile-switch-project  ; Switch project
 )
(project-leader-def
 :states '(normal visual)
 "g" 'helm-projectile-find-file-dwim ; Find file at point
 )


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


; TODO improve managing sections movement and folding keybinds
; such that they are uniform with org-mode




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

;;; Wiki
;; __        ___ _    _ 
;; \ \      / (_) | _(_)
;;  \ \ /\ / /| | |/ / |
;;   \ V  V / | |   <| |
;;    \_/\_/  |_|_|\_\_|
;;                      
(require 'org)
(require 'helm-org)

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
;; Define the differents stages of a TODO
;; Multiple sequences can be defined
;; TODO define meaningful sequences
(setq org-todo-keywords
      '((sequence "IDEA(i!)" "TODO(t!)" "INPROGRESS(i!)" "PAUSED(p!)" "|" "DONE(d@)")
	(sequence "|" "CANCELED(c@)"))) 
;; Colors and decorations for stages of TODO
(setq org-todo-keyword-faces
      `(("IDEA"       . (:foreground ,dwarfmaster/ca
			 :background ,dwarfmaster/c2
			 :weight bold))
	("TODO"       . (:foreground ,dwarfmaster/c8
			 :background ,dwarfmaster/c2
			 :weight bold))
	("INPROGRESS" . (:foreground ,dwarfmaster/c1
			 :background ,dwarfmaster/ce
			 :weight bold))
	("PAUSED"     . (:foreground ,dwarfmaster/c9
			 :background ,dwarfmaster/c2
			 :weight bold))
	("DONE"       . (:foreground ,dwarfmaster/cc
			 :background ,dwarfmaster/c2
			 :weight bold))
	("CANCELED"   . (:foreground ,dwarfmaster/c1
			 :background ,dwarfmaster/cf))))
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
;; Set default notes file for capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; Always set the org-capture-last-stored bookmark
(setq org-capture-bookmark t)
;; When opening agenda, open it in another window next ot current one
(setq org-agenda-window-setup 'reorganize-frame)
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
;; Display inline images
(setq org-startup-with-inline-images t)
;; Ask for confirmation before running babel, shell link or elisp link
(setq org-confirm-babel-evaluate t)
(setq org-confirm-elisp-link-function 'yes-or-no-p)
(setq org-confirm-shell-link-function 'yes-or-no-p)

;; TODO setup LaTeX preview
;; TODO configure capture
(fancy-leader-def
  :states 'normal
  "l"   'org-store-link
  "c"   'org-capture
  )
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "RET"  'org-open-at-point
 "TAB"  'org-cycle
 "DEL"  'org-mark-ring-goto  ; Jump to previous org mark
 )
(org-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "Z"   'org-set-startup-visibility     ; Reset visibility to start
 "z"   'org-reveal                     ; Reveal context around point  
 "U"   'outline-show-all               ; Unfold everything
 "O"   'org-tree-to-indirect-buffer    ; Open current subtree in an indirect buffer
 "h"   'outline-up-heading             ; Move to the buffer above
 "j"   'org-next-visible-heading       ; Move to next heading
 "k"   'org-previous-visible-heading   ; Move to previous heading
 "s"   'helm-org-in-buffer-heading     ; Find heading in buffer
 "RET" 'org-meta-ret                   ; Insert new heading/item/table row
 "<"   'org-do-promote                 ; Promote current heading by a level
 ">"   'org-do-demote                  ; Demote current heading by a level
 "@"   'org-mark-subtree               ; Select current subtree
 "/"   'org-sparse-tree                ; Create sparse tree for match
 "n"   'next-error                     ; Jump to next sparse tree match
 "p"   'previous-error                 ; Jump to previous sparse tree match
 "%"   'org-mark-ring-push             ; Push current position in org mark ring
 "t"   'org-todo                       ; Toggle the todo mark along a sequence
 "T"   'org-todo-list                  ; Display the window as sparse tree with
				       ; only TODO headers
 "+"   'org-update-statistics-cookies  ; Update statistics for current entry
 "#"   '(lambda () (interactive) (org-update-statistics-cookies t))
                                       ; Update statistics in the whole file
 "c"   'org-toggle-checkbox            ; Toggle checkbox state
 "P"   'org-priority                   ; Set priority for current TODO item
 "'"   'org-set-tags-command           ; Set tags for current heading
 "u"   'org-dblock-update              ; Update dblock at point
 "U"   'org-update-all-dblocks         ; Update all dblocks in file
 "e"   'org-set-effort                 ; Set the effort estimate for a task
 "p"   'org-toggle-pretty-entities     ; Toggle the pretifying
 "E"   'org-edit-special               ; Edit the source code example at point in its native mode
 )
(org-leader-def
 :states '(normal visual)
 :keymaps 'org-mode-map
 "S"   'org-sort              ; Sort same-level entries
 "*"   'org-toggle-heading    ; Toggle heading on line (or selected lines)
 "L"   'org-insert-link       ; Insert link at current position (or over current text)
 )
;; Enable the org table editor mode on other modes by calling 'turn-on-orgtbl when
;; loading other modes. Can be automated with
;;    (add-hook 'message-mode-hook 'turn-on-orgtbl)
(org-table-leader-def
 :states '(normal visual)
 :keymaps 'orgtbl-mode-map
 "C"   'org-table-create-or-convert-from-region  ; Convert region to table
 "a"   'org-table-align          ; Re-align table
 "h"   'org-table-previous-field
 "j"   'org-table-next-row
 ;; TODO find a previous row command
 "l"   'org-table-next-field
 "^"   'org-table-beginning-of-field
 "$"   'org-table-end-of-field
 "i"   'org-table-import          ; Import file as table
 ":"   'org-table-export          ; Export a table as file
 "z"   'org-table-shrink          ; Shrink all columns with size
 "Z"   'org-table-expand          ; Expand all columns
 "TAB" 'org-table-toggle-column-width
 "}"   'org-table-toggle-coordinate-overlays ; Display an overlay with fields coordinates

 ;; Editing commands
 "H"   'org-table-move-column-left
 "J"   'org-table-move-row-down
 "K"   'org-table-move-row-up
 "L"   'org-table-move-column-right
 "i"   'org-table-insert-column
 "r"   'org-table-insert-row
 "-"   'org-table-insert-hline
 "d"   'org-table-kill-row
 "S"   'org-table-sort-lines
 "e"   'org-table-edit-field      ; Edit field in another buffer
 "C-a" 'org-table-copy-down       ; Copy current field down, incrementing integers
 "="   'org-table-eval-formula    ; Insert formula for current field

 ;; Cut-copy-paste
 "x"   'org-table-cut-region
 "y"   'org-table-copy-region
 "p"   'org-table-paste-rectangle

 ;; Calculations
 "+"   'org-table-sum         ; Sum all numbers in current region or column
 "*"   'org-table-recalculate ; Recalculate current row
 "#"   'org-table-iterate     ; Recalculate current table until it stabilies
 "|"   'org-table-recalculate-buffer-tables ; Recalculate all tables in buffer
 )
(org-displace-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "<"    'org-promote-subtree      ; Promote current subtree
 ">"    'org-demote-subtree       ; Demote current subtree
 "J"    'org-move-subtree-down
 "K"    'org-move-subtree-up
 "d"    'org-cut-subtree          ; Cut current subtree
 "y"    'org-copy-subtree         ; Copy current subtree
 "p"    'org-paste-subtree        ; Cleverly paste current subtree, adapting level
 "j"    'org-move-item-down       ; Move list item down
 "k"    'org-move-item-up         ; Move list item up
 "A"    'org-archive-subtree      ; Archive subtree
 ;; TODO find commands to shift indents in list item and bind them to h,l
 ;; TODO find commands to change item bullet and bind them to *
 ;; TODO investigate refiling mecanisms
 )
(org-insert-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "h"    'org-insert-heading                      ; Insert heading at point
 "H"    'org-insert-heading-respect-content      ; Insert heading after subtree
 "t"    'org-insert-todo-heading                 ; Insert todo at point
 "T"    'org-insert-todo-heading-respect-content ; Insert todo after subtree
 "s"    'org-time-stamp                          ; Insert timestamp
 "S"    'org-time-stamp-inactive                 ; Insert inactive timestamp
 "d"    'org-schedule                            ; Insert schedule entry
 "D"    'org-deadline                            ; Insert deadline entry
 )
(org-property-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "s"    'org-set-property               ; Set a property
 "i"    'org-insert-drawer              ; Insert a property drawer for current entry
 "a"    'org-property-action            ; Execute property command
 "c"    'org-compute-property-at-point  ; Compute property at point
 "h"    'org-property-previous-allowed-value
 "l"    'org-property-next-allowed-value
 "d"    'org-delete-property            ; Remove a property from current entry
 "D"    'org-delete-property-globally   ; Remove a property from all entries in current file
 "C"    'org-columns                    ; Start column view
 "b"    'org-insert-columns-dblock      ; Insert a column dynamic block
 )
(clock-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 ;; Clock
 "s"    'org-clock-in                     ; Start the clock on the current item
 "u"    'org-evaluate-time-range          ; Recompute the time interval after editing a timestamp
 "j"    'org-clock-timestamps-down        ; Decrease both start and end of a CLOCK line by same duration
 "k"    'org-clock-timestamps-up          ; Decrease both start and end of a CLOCK line by same duration
 "C"    'org-clock-cancel                 ; Cancel current clock
 "d"    'org-clock-display                ; Display overlay with clock information on header lines
 "e"    'org-clock-modify-effort-estimate ; Modify the effort estimate of the clocked item

 ;; timer
 "t"    'org-timer                        ; Print relative timer value, start it if it wasn't started
 "T"    'org-timer-set-timer              ; Start a decreasing timer
 "-"    'org-timer-item                   ; Insert list item with relative timer value
 "r"    'org-timer-start                  ; (Re)Start relative timer with value 0
 "R"    'org-timer-stop                   ; Stop relative timer
 "p"    'org-timer-pause-or-continue      ; Pause/Continue relative timer
 )
(clock-leader-def
 :states 'normal
 "e"    'org-clock-out       ; Stop the clock
 "S"    'org-clock-in-last   ; Re-clock the last closed task
 )
(org-agenda-leader-def
 :states '(normal visual)
 "o"     'org-agenda                        ; Open the agenda dispatcher
 "w"     'org-agenda-list                   ; Open the agenda for the current week
 "t"     'org-todo-list                     ; Open the global todo list
 "T"     '(lambda () (interactive) (org-tags-view 'TODO-ONLY))
                                            ; Select todos matching specific tags
 "s"     'org-tags-view                     ; List all headers matching a tag
 "S"     'org-search-view                   ; General text search ability for org mode entries
 ;; TODO investigate stuck projects
 )
(org-agenda-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "A"    'org-agenda-file-to-front           ; Add current file to list of agendas
 "D"    'org-remove-file                    ; Remove current file from list of agendas
 "n"    'org-cycle-agenda-files             ; Move to the next agenda file
 ;; TODO bind p to the previous agenda file
 "<"    'org-agenda-set-restriction-lock    ; Restrict agenda to current subtree
 ">"    'org-agenda-remove-restriction-lock ; Remove restriction lock
 )
(language-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "r"    'org-babel-execute-src-block       ; Execute block at point
  "R"    'org-babel-execute-buffer          ; Execute all blocks in buffer
  "t"    'org-babel-tangle                  ; Tangle block at point
  "T"    'org-babel-tangle-file             ; Tangle buffer
  "n"    'org-babel-next-src-block          ; Move to next block
  "p"    'org-babel-previous-src-block      ; Move to previous src block
  "g"    'org-babel-goto-src-block-result   ; Go to block result
  "z"    'org-babel-expand-src-block        ; Unfold src block
  )


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
(language-leader-def
  :states 'normal
  :keymaps 'nix-mode-map
  "r" 'nix-repl ; TODO open repl in new window
  )
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
(language-leader-def
  :states 'normal
  :keymaps 'hledger-mode-map
  "r" 'hledger-run-command)
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
(language-leader-def
  :states 'normal
  :keymaps 'idris-mode-map
  "h" 'helm-idris                 ; Load a helm based idris doc looker
  "r" 'idris-load-file            ; Interpret current file in buffer
  "R" 'idris-pop-to-repl          ; Pop up the clause window
  "M" 'idris-add-clause           ; Add missing pattern match clause
  "m" 'idris-case-split           ; Case split variable under cursor
  "t" 'idris-type-at-point        ; Get the type of the expession under point
  "A" 'idris-toggle-term-widgets  ; Show active terms
  "N" 'idris-normalize-term       ; Normalize term at point
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

; Keybinds
(language-leader-def
  :states 'normal
  :keymaps 'coq-mode-map
  "R" 'proof-layout-windows                  ; Reset window layout
  "e" 'proof-goto-end-of-locked              ; Move point to end of prooved region
  "r" '(lambda ()                            ; Start PG on current buffer (and stop on any previous buffer it was on)
	 (interactive)
	 (proof-toggle-active-scripting 1))
  "R" 'proof-display-some-buffers            ; Rotate through buffers
  "h" 'proof-goto-command-start              ; Move to beggining of command at point
  "l" 'proof-goto-command-end                ; Move to end of command at point
  "j" 'proof-assert-next-command-interactive ; Interpret next command
  "k" 'proof-undo-last-successful-command    ; Undo last command
  "J" 'proof-goto-point                      ; Interpret all command up to point, or retract to point
  "K" 'proof-undo-and-delete-last-successful-command
  "h" 'dwarfmaster/coq-man                   ; Lookup for symbol/theorem with helm
  "v" 'pg-toggle-visibility                  ; Toggle visibility of proof body under point
  "V" 'dwarfmaster/toggle-all-proofs         ; Toggle visibility of all completed proofs body at once
  )



