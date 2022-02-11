
;;; Code:

;;; Basic UI
(map! :leader "b" 'helm-mini)

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

;; Spell checker
;;  ____             _ _    ____ _               _
;; / ___| _ __   ___| | |  / ___| |__   ___  ___| | _____ _ __
;; \___ \| '_ \ / _ \ | | | |   | '_ \ / _ \/ __| |/ / _ \ '__|
;;  ___) | |_) |  __/ | | | |___| | | |  __/ (__|   <  __/ |
;; |____/| .__/ \___|_|_|  \____|_| |_|\___|\___|_|\_\___|_|
;;       |_|
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

;; Authentification sources
;;     _         _   _                _   _  __ _           _   _
;;    / \  _   _| |_| |__   ___ _ __ | |_(_)/ _(_) ___ __ _| |_(_) ___  _ __
;;   / _ \| | | | __| '_ \ / _ \ '_ \| __| | |_| |/ __/ _` | __| |/ _ \| '_ \
;;  / ___ \ |_| | |_| | | |  __/ | | | |_| |  _| | (_| (_| | |_| | (_) | | | |
;; /_/   \_\__,_|\__|_| |_|\___|_| |_|\__|_|_| |_|\___\__,_|\__|_|\___/|_| |_|

;; For now we dont want any authenfication source
;; Including no password-store
(after! password-store
  (setq auth-sources '()))

;; Project root
;;  ____            _           _     ____             _
;; |  _ \ _ __ ___ (_) ___  ___| |_  |  _ \ ___   ___ | |_
;; | |_) | '__/ _ \| |/ _ \/ __| __| | |_) / _ \ / _ \| __|
;; |  __/| | | (_) | |  __/ (__| |_  |  _ < (_) | (_) | |_
;; |_|   |_|  \___// |\___|\___|\__| |_| \_\___/ \___/ \__|
;;               |__/
(defun dwarfmaster/projectile-find-root (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(after! project
  (add-to-list 'project-find-functions 'dwarfmaster/projectile-find-root))
