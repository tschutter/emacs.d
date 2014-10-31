;;; url-humanify.el --- break an URL up to make it readable

;;; Commentary:

;; Modified from
;; http://www.blogbyben.com/2013/09/emacs-function-humanifying-urls.html

;;; Code:

(defun url-humanify ()
  "Take the URL at point and make it human readable."
  (interactive)
  (let* ((area (bounds-of-thing-at-point 'url))
         (num-params (count-matches "&" (car area) (cdr area)))
         (i 0))
    (beginning-of-thing 'url)
    (when (search-forward "?" (cdr area) t nil)
      (insert "\n  ")
      (while (< i num-params)
        (search-forward "&" nil t nil)
        (insert "\n  ")
        (save-excursion
          (forward-line -1)
          (let ((start (search-forward "="))
                (end (search-forward "&")))
            (url-decode-region start end)))
        (setq i (1+ i))))))

(defun url-decode-region (start end)
  "Replace a region from START to END with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(provide 'url-humanify)
;;; url-humanify ends here
