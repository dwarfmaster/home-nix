
;; Org Attach
;;   ___                 _   _   _             _
;;  / _ \ _ __ __ _     / \ | |_| |_ __ _  ___| |__
;; | | | | '__/ _` |   / _ \| __| __/ _` |/ __| '_ \
;; | |_| | | | (_| |  / ___ \ |_| || (_| | (__| | | |
;;  \___/|_|  \__, | /_/   \_\__|\__\__,_|\___|_| |_|
;;            |___/
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
