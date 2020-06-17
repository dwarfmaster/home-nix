;;; ob-hledger.el --- org-babel functions for hledger evaluation

;; Copyright (C) 2010-2016 Simon Michael
;; (Free Software Foundation, Inc.)

;; Author: Simon Michael
;; Keywords: literate programming, reproducible research, accounting
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating hledger entries.
;;
;; Based on ob-ledger.el.  
;; If the source block is empty, hledger will use the default journal file.

;;; Code:
(require 'ob)

(require 'nixpaths)

(defvar org-babel-default-header-args:hledger
  '((:results . "output") (:cmdline . "bal"))
  "Default arguments to use when evaluating a hledger source block.")

(defun org-babel-execute:hledger (body params)
  "Execute a block of hledger entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing hledger source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
	(cmdline (cdr (assoc :cmdline params)))
        (in-file (org-babel-temp-file "hledger-"))
	(out-file (org-babel-temp-file "hledger-output-")))
    (with-temp-file in-file (insert body))
    (message "%s" (concat "hledger"
                          (if (> (length body) 0)
                              (concat " -f " (org-babel-process-file-name in-file))
                            "")
                          " " cmdline))
    (with-output-to-string
      (shell-command (concat nix/hledger
                             (if (> (length body) 0)
                                 (concat " -f " (org-babel-process-file-name in-file))
                               "")
                             " " cmdline
                             " > " (org-babel-process-file-name out-file))))
    (with-temp-buffer (insert-file-contents out-file) (buffer-string))))

(defun org-babel-prep-session:hledger (session params)
  (error "hledger does not support sessions"))

(provide 'ob-hledger)



;;; ob-hledger.el ends here
