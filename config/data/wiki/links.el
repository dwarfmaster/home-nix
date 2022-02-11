
;; Attach links
;;     _   _   _             _
;;    / \ | |_| |_ __ _  ___| |__
;;   / _ \| __| __/ _` |/ __| '_ \
;;  / ___ \ |_| || (_| | (__| | | |
;; /_/   \_\__|\__\__,_|\___|_| |_|

(cl-defgeneric dwarfmaster/attach-export (path desc backend)
  "Generic function to export an attachment link DESC to PATH, for BACKEND."
  desc)
(after! org
  (org-link-set-parameters "attachment" :export 'dwarfmaster/attach-export))

(cl-defmethod dwarfmaster/attach-export ((path t) (desc t) (backend (eql md)))
  (format "🗃%s🗄" desc))

;; Cite links
;;   ____ _ _
;;  / ___(_) |_ ___
;; | |   | | __/ _ \
;; | |___| | ||  __/
;;  \____|_|\__\___|

(cl-defgeneric dwarfmaster/cite-export (path desc backend)
  "Generic function to export a cite link DESC to PATH, for BACKEND."
  desc)
(after! org
  (org-link-set-parameters "cite" :export 'dwarfmaster/cite-export))

(cl-defmethod dwarfmaster/cite-export ((path t) (desc t) (backend (eql md)))
  (format "📜%s" desc))
