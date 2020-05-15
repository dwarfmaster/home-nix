
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



;;; Interface
;;  ___       _             __                
;; |_ _|_ __ | |_ ___ _ __ / _| __ _  ___ ___ 
;;  | || '_ \| __/ _ \ '__| |_ / _` |/ __/ _ \
;;  | || | | | ||  __/ |  |  _| (_| | (_|  __/
;; |___|_| |_|\__\___|_|  |_|  \__,_|\___\___|
;;                                            

;; Color scheme, using the base16 package
;; https://github.com/belak/base16-emacs
(require 'base16-theme)
(load-theme 'base16-woodland t)
(defvar my/base16-colors base16-woodland-colors)

;; Set the cursor color based on the evil state
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; Configure the GUI
(if window-system
  (progn (scroll-bar-mode -1) ; Hide the scroll bar
         (menu-bar-mode -1)   ; Hide the menu bar
         (tool-bar-mode -1)   ; Hide the toolbar
  )
)

;; Set the font
(add-to-list 'default-frame-alist
             '(font . "FuraCode Nerd Font Mono-12:weight=bold"))

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



