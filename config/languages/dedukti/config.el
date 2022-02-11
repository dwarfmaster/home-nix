
;; Helpers
;; Taken from https://www.emacswiki.org/emacs/misc-cmds.el
(defun kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing) 'MSGP))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when msgp (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

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
      "R" #'lambdapi-eglot-reconnect
      (:prefix ("g" . "goto")
       "n" #'lp-jump-proof-forward
       "p" #'lp-jump-proof-backward))
;; I am only interested in syntactic coloration for now
;; TODO find how to kill eglot
(defun clean-lambdapi-mode ()
  (kill-buffer-and-its-windows "*Goals*")
  (kill-buffer-and-its-windows "*lp-logs*"))
(after! lambdapi-mode
  (add-hook! 'lambdapi-mode-hook #'clean-lambdapi-mode))
