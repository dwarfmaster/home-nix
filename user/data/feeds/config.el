

;;  _____ _  __               _
;; | ____| |/ _| ___  ___  __| |
;; |  _| | | |_ / _ \/ _ \/ _` |
;; | |___| |  _|  __/  __/ (_| |
;; |_____|_|_|  \___|\___|\__,_|

(setq rmh-elfeed-org-files (list "~/wiki/support/feeds.org"))
(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +unread -webcomics"))
(map! :leader "o l" 'elfeed)


;; __        __   _                         _
;; \ \      / /__| |__   ___ ___  _ __ ___ (_) ___
;;  \ \ /\ / / _ \ '_ \ / __/ _ \| '_ ` _ \| |/ __|
;;   \ V  V /  __/ |_) | (_| (_) | | | | | | | (__
;;    \_/\_/ \___|_.__/ \___\___/|_| |_| |_|_|\___|
;;
;;  ____
;; / ___| _   _ _ __ ___  _ __ ___   __ _ _ __ _   _
;; \___ \| | | | '_ ` _ \| '_ ` _ \ / _` | '__| | | |
;;  ___) | |_| | | | | | | | | | | | (_| | |  | |_| |
;; |____/ \__,_|_| |_| |_|_| |_| |_|\__,_|_|   \__, |
;;                                             |___/
(defun dwarfmaster/elfeed/clean-title (title)
  "Replace all | by - in TITLE."
  (replace-regexp-in-string "|" "-" title))

(defun dwarfmaster/elfeed/prepare-webcomic (feed result)
  "Add basic information associated to FEED in RESULT."
  (let ((id    (elfeed-feed-id feed))
        (title (dwarfmaster/elfeed/clean-title (elfeed-feed-title feed))))
    (when (not (assoc id result))
      (push `(,id . ,(list title nil 0)) result))
    result))

(defun dwarfmaster/elfeed/add-unread (feed result)
  "Increment by one the unread count associated to FEED in RESULT."
  (let* ((id     (elfeed-feed-id feed))
         (rentry (alist-get id result)))
    (setf (nth 2 rentry) (+ 1 (nth 2 rentry)))
    (setf (alist-get id result) rentry)))

(defun dwarfmaster/elfeed/set-last-read (feed entry result)
  "If the last read for FEED is not set, set it to ENTRY in RESULT."
  (let* ((id     (elfeed-feed-id feed))
         (lread  (nth 1 (alist-get id result)))
         (rentry (alist-get id result)))
    (when (not lread)
      (setf (nth 1 rentry) (elfeed-entry-link entry))
      (setf (alist-get id result) rentry))))

(defun dwarfmaster/elfeed/webcomics (tag)
  "Return webcomics name, last read and number of unreads from TAG."
  (let ((result nil))
    (with-elfeed-db-visit (entry feed)
      (let* ((tags        (elfeed-entry-tags entry))
             (is-webcomic (and (member 'webcomics tags)
                               (member tag        tags)))
             (is-unread   (member 'unread tags)))
        (when is-webcomic
          (setq result (dwarfmaster/elfeed/prepare-webcomic feed result))
          (if is-unread
              (dwarfmaster/elfeed/add-unread feed result)
            (dwarfmaster/elfeed/set-last-read feed entry result)))))
    result))

(defun dwarfmaster/elfeed/mark-feed-as-read (feed-id)
  "Mark all entries of the feed identified by FEED-ID as read."
  (let ((entries (elfeed-feed-entries feed-id)))
    (elfeed-untag entries 'unread)))

(defun dwarfmaster/elfeed/pp-row (row)
  "Pretty print a ROW."
  (let* ((id      (car row))
         (title   (cadr row))
         (last    (nth 1 (cdr row)))
         (unreads (nth 2 (cdr row))))
    (format-message "| [[%s][%s]] | %d | [[elisp:(dwarfmaster/elfeed/mark-feed-as-read \"%s\")][Mark as read]] |\n"
                    last title
                    unreads
                    id)))

(defun dwarfmaster/elfeed/webcomics-summary (tag)
  "Make summary of webcomics with TAG."
  (let* ((webs (sort (dwarfmaster/elfeed/webcomics tag)
                     (lambda (x y) (< (nth 2 (cdr y))
                                      (nth 2 (cdr x)))))))
    (concat "|--|\n| Webcomic | Unreads | Action |\n|--|\n"
            (apply 'concat (mapcar 'dwarfmaster/elfeed/pp-row webs))
            "|--|\n")))

(defun org-dblock-write:webcomics-summary (params)
  (let* ((tag     (or (plist-get params :tag) 'webcomics))
         (summary (dwarfmaster/elfeed/webcomics-summary tag)))
    (insert summary)
    (org-table-align)))
