
;; Language: ledger
;;  _             _
;; | |    ___  __| | __ _  ___ _ __
;; | |   / _ \/ _` |/ _` |/ _ \ '__|
;; | |__|  __/ (_| | (_| |  __/ |
;; |_____\___|\__,_|\__, |\___|_|
;;                  |___/

;; TODO move to hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))
;; Configure ledger-mode to work with hledger
(setq ledger-mode-should-check-version nil
      ledger-report-links-in-register nil
      ledger-binary-path "hledger")

