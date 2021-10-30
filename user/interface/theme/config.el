
;;  _____ _
;; |_   _| |__   ___ _ __ ___   ___
;;   | | | '_ \ / _ \ '_ ` _ \ / _ \
;;   | | | | | |  __/ | | | | |  __/
;;   |_| |_| |_|\___|_| |_| |_|\___|
;;

(setq doom-font "FiraCode Nerd Font Mono-11:weight=bold")

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(solaire-global-mode +1)
(setq fancy-splash-image "~/wiki/logo.png")

(after! doom-themes
  ;; Set the theme after doom-themes is loaded
  (setq doom-theme 'doom-manegarm)
  ;; Enable flashing modeline on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neatree theme
  (doom-themes-neotree-config)
  ;; Corrects org native fontification
  (doom-themes-org-config))

;;; Org UI
;;   ___              _   _ ___
;;  / _ \ _ __ __ _  | | | |_ _|
;; | | | | '__/ _` | | | | || |
;; | |_| | | | (_| | | |_| || |
;;  \___/|_|  \__, |  \___/|___|
;;            |___/
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
