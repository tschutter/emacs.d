;;; $Id: pra-code-fixup.el,v 1.8 2003/05/29 20:03:30 tom Exp $
;;
;; Allows updating the address and copyright year manually or when
;; saving a file.
;;
;; Add this to your .emacs:
;;   (add-hook 'write-file-hooks 'pra-header-update).
;;

;;; Code:

(defvar pra-author nil
  "*Name that pra-author-list-update will append to end of the author list.")

(defvar pra-address-update-query nil
  "*If non-`nil', ask user before changing address.
When this is `function', only ask when called non-interactively.")

(defvar pra-copyright-update-query nil
  "*If non-`nil', ask user before changing copyright.
When this is `function', only ask when called non-interactively.")

(defvar pra-author-list-fixup-query nil
  "*If non-`nil', ask user before fixing up author list.
When this is `function', only ask when called non-interactively.")

(defvar pra-author-list-update-query t
  "*If non-`nil', ask user before appending to the author list.
When this is `function', only ask when called non-interactively.")

(defvar pra-header-limit 400
  "*Don't try to update header beyond this position unless interactive.
`nil' means to search whole buffer.")

(defvar pra-address-update t)
(defvar pra-address-fixup t)
(defvar pra-copyright-update t)
(defvar pra-author-list-fixup t)
(defvar pra-author-list-update t)

;; What the old address looks like.
(setq pra-old-address-regexp
      "\\( *[\\*#;%]\\) 2000 West 120th, Suite 10
 *[\\*#;%] Denver, CO 80234")

(defun pra-address-update ()
  "Update the address notice at the beginning of the buffer to indicate
the new address."
  (interactive)
  (if pra-address-update
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (re-search-forward pra-old-address-regexp pra-header-limit t)
              (let ((comment-char (match-string 1))
                    (match-start (match-beginning 0))
                    (match-end (match-end 0)))
                (if (or (not pra-address-update-query)
                        (and (eq pra-address-update-query 'function)
                             (eq this-command 'pra-address-update))
                        (y-or-n-p
                         "Change address from Denver to Boulder? "))
                    (progn
                      (delete-region match-start match-end)
                      (insert (concat comment-char " 2790 Valmont Road\n"
                                      comment-char " Boulder, CO 80304\n"
                                      comment-char " (303) 448-0480\n"
                                      comment-char " www.platte.com\n"
                                      comment-char))))))))
    (set (make-local-variable 'pra-address-update) nil))
  ;; If a write-file-hook returns non-nil, the file is presumed to be written.
  nil)

;; What the new address looks like.
(setq pra-address-regexp
      "\\( *[\\*#;%]\\) Boulder, CO 80304
")

;; What the new phone number looks like.
(setq pra-phone-regexp
      " (303) 448-0480
")

(defun pra-address-fixup ()
  "Fixup the address notice at the beginning of the buffer."
  (interactive)
  (if pra-address-fixup
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (re-search-forward pra-address-regexp pra-header-limit t)
              (let ((comment-char (match-string 1))
                    (comment-char-regexp (match-string 1))
                    (match-end (match-end 0))
                    (phone-regexp "")
                    (url-regexp ""))
                (while (string-match "\\(^\\|[^\\\]\\)\\(\\*\\)"
                                     comment-char-regexp)
                  (setq comment-char-regexp
                        (replace-match "\\*" t t comment-char-regexp 2)))
                (setq phone-regexp (concat
                                    "^"
                                    comment-char-regexp
                                    pra-phone-regexp))
                (setq url-regexp (concat
                                  "^"
                                  comment-char-regexp
                                  " www.platte.com"))
                (if (not (re-search-forward phone-regexp pra-header-limit t))
                  (insert (concat comment-char " (303) 448-0480\n")))
                (if (not (re-search-forward url-regexp pra-header-limit t))
                  (insert (concat comment-char " www.platte.com\n"
                                  comment-char "\n")))
                ))))
    (set (make-local-variable 'pra-address-fixup) nil))
  ;; If a write-file-hook returns non-nil, the file is presumed to be written.
  nil)

;; What the copyright notice can look like.
;; The second \\( \\) construct must match the begin year.
;; The third \\( \\) construct must match the final year.
(setq pra-copyright-regexp
      "\\(?:[Cc]opyright\\s *\\)*\\(?:([Cc])\\)\
\\s *\\([1-9][0-9]+\\)\
\\([\\s ,-]+[1-9][0-9]+\\)*")

;; This is a defvar rather than a defconst, because the year can
;; change during the Emacs session.
(defvar pra-copyright-current-year "2003"
  "String representing the current year.")

(defun pra-copyright-update ()
  "Update the copyright notice at the beginning of the buffer to indicate
the current year."
  (interactive)
  (if pra-copyright-update
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (setq pra-copyright-current-year
                (substring (current-time-string) -4))
          (if (re-search-forward pra-copyright-regexp pra-header-limit t)
              (if (and
                   (match-end 2)
                   (string=
                    (buffer-substring (- (match-end 2) 2) (match-end 2))
                           (substring pra-copyright-current-year -2)))
                  ()
                (let ((begin-year (match-string 1))
                      (match-start (match-beginning 0))
                      (match-end (match-end 0)))
                  (if (or (not pra-copyright-update-query)
                          (and (eq pra-copyright-update-query 'function)
                               (eq this-command 'pra-copyright-update))
                          (y-or-n-p (concat "Append year "
                                            pra-copyright-current-year
                                            " to copyright? ")))
                      (progn
                        (delete-region match-start match-end)
                        (insert (concat "Copyright (c) "
                                        begin-year "-"
                                        pra-copyright-current-year))))))))
        (set (make-local-variable 'pra-copyright-update) nil)))
  ;; If a write-file-hook returns non-nil, the file is presumed to be written.
  nil)

;; What a bad author list header looks like.
(setq pra-bad-author-list-regexp
      "\\(^ *[\\*#;%]\\) Author\\(?:(s)\\|s\\)*: \\(.+\\)")

(defun pra-author-list-fixup ()
  "Fixup author list"
  (interactive)
  (if pra-author-list-fixup
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (re-search-forward pra-bad-author-list-regexp pra-header-limit t)
              (let ((comment-char (match-string 1))
                    (match-start (match-beginning 0))
                    (match-end (match-end 0))
                    (first-author (match-string 2))
                    (comment-char-regexp (match-string 1))
                    (second-author-regexp ""))
                (while (string-match "\\(^\\|[^\\\]\\)\\(\\*\\)"
                                     comment-char-regexp)
                  (setq comment-char-regexp
                        (replace-match "\\*" t t comment-char-regexp 2)))
                (setq second-author-regexp (concat
                                            "^"
                                            comment-char-regexp
                                            "\\s +\\(\\sw.*\\)"))
                (if (or (not pra-author-list-fixup-query)
                        (and (eq pra-author-list-fixup-query 'function)
                             (eq this-command 'pra-author-list-fixup))
                        (y-or-n-p
                         "Reformat author list? "))
                    (progn
                      (delete-region match-start match-end)
                      (insert (concat comment-char " Author(s):\n"
                                      comment-char "   " first-author))
                      (while (re-search-forward second-author-regexp
                                                pra-header-limit t)
                        (let ((second-author (match-string 1)))
                          (delete-region (match-beginning 0) (match-end 0))
                          (insert
                           (concat comment-char "   " second-author))
                          )
                        )
                      )
                  ))
            (if (re-search-forward " Author\\(s\\):$" pra-header-limit t)
              (let ((plural-start (match-beginning 1))
                    (plural-end (match-end 1)))
                (if (or (not pra-author-list-fixup-query)
                        (and (eq pra-author-list-fixup-query 'function)
                             (eq this-command 'pra-author-list-fixup))
                        (y-or-n-p
                         "Reformat author header? "))
                    (progn
                      (goto-char plural-start)
                      (delete-region plural-start plural-end)
                      (insert "(s)"))
                  )))
            )
          ))
    (set (make-local-variable 'pra-author-list-fixup) nil))
  ;; If a write-file-hook returns non-nil, the file is presumed to be written.
  nil)

;; What a good author list header looks like.
(setq pra-author-list-regexp "\\(^ *[\\*#;%]\\) Author(s):$")

(defun pra-author-list-update ()
  "Append user name to end of author list"
  (interactive)
  (if (and pra-author-list-update pra-author)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (re-search-forward pra-author-list-regexp pra-header-limit t)
              (let ((comment-char (match-string 1))
                    (comment-char-regexp (match-string 1))
                    (author-regexp "")
                    (found nil))
                (while (string-match "\\(^\\|[^\\\]\\)\\(\\*\\)"
                                     comment-char-regexp)
                  (setq comment-char-regexp
                        (replace-match "\\*" t t comment-char-regexp 2)))
                (setq author-regexp (concat
                                     "^"
                                     comment-char-regexp
                                     "   \\(\\sw.*\\)"))
                (while (re-search-forward author-regexp pra-header-limit t)
                  (if (string= (match-string 1) pra-author) (setq found t)))
                (if (not found)
                    (if (or (not pra-author-list-update-query)
                            (and (eq pra-author-list-update-query 'function)
                                 (eq this-command 'pra-author-list-update))
                            (y-or-n-p
                             (concat
                              "Append " pra-author " to the author list? ")))
                        (insert
                         (concat "\n" comment-char "   " pra-author)))
                  )))))
    (set (make-local-variable 'pra-author-list-update) nil))
  ;; If a write-file-hook returns non-nil, the file is presumed to be written.
  nil)

(defun pra-header-update ()
  "Update the address and copyright notice at the beginning of the buffer."
  (interactive)
  (pra-address-update)
  (pra-address-fixup)
  (pra-copyright-update)
  (pra-author-list-fixup)
  (pra-author-list-update))

(defun pra-code-fixup ()
  "Apply all pra code fixups."
  (interactive)
  (pra-header-update))

(define-skeleton pra-copyright
  "Insert a copyright by $ORGANIZATION notice at cursor."
  "Company: "
  comment-start
  "Copyright (C) " `(substring (current-time-string) -4) " by "
  (or (getenv "ORGANIZATION")
      str)
  '(if (> (point) pra-header-limit)
       (message "Copyright extends beyond `pra-header-limit' and won't be updated automatically."))
  comment-end)

(provide 'pra-code-fixup)
