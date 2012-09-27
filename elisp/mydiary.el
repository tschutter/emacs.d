;; mydiary.el -- Alternate diary handling

;; To use, add this to your .emacs:
;; (load-library "mydiary")

;; use time-stamp-format from time-stamp.el
(require 'time-stamp)

(defun mydiary-new-entry ()
  "Append timestamp to end of diary."
  (interactive)
  (let ((time-stamp-format "\n-- %:y-%02m-%02d %02H:%02M %:a\n"))
    (end-of-buffer)
    (insert (time-stamp-string))))

(defun mydiary-load ()
  "Load mydiary."
  (interactive)
  (find-file "~/doc/diary/mydiary.gpg")
  (mydiary-new-entry))

(define-key global-map [f12] 'mydiary-load)
