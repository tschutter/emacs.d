;;; init.el --- emacs(1) config file

;;; Commentary:

;;; Code:

;;;; Turn off tool bar early in startup to avoid momentary display.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;;; Configure some standard directory names.
;;; Determine the location of the .emacs.d directory.
(defvar emacs-d-directory (file-name-directory load-file-name))
;;; Location for state and cache files.
(defvar emacs-var-directory (expand-file-name "~/.var/emacs/"))
(make-directory emacs-var-directory t)  ;create it if it does not exist
(set-file-modes emacs-var-directory #o700)  ;and make it private

;;;; emacs-d-directory/elisp/
;;; Some packages installed in emacs-d-directory/elisp/ are single
;;; files while others are placed inside their own sub-directories.
;;; Prepend emacs-d-directory/elisp/ and all of it's subdirectories to
;;; load-path.
;;; See http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
(let ((default-directory (concat emacs-d-directory "elisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;;;; Emacs Lisp Packages
;;; "M-x list-packages" to see available and installed list.
;;; "M-x package-install" to install a new package.
(when (require 'package nil :noerror)
  ; Milkypostman’s Emacs Lisp Package Archive
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  ;; Activate installed packages.
  (package-initialize)

  ;; Automatically install packages.
  ;; http://stackoverflow.com/questions/10092322
  (defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.

     Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       ;; (package-installed-p 'evil)
       (if (package-installed-p package)
           nil
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package)
           package)))
     packages))

  ;; Make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  (ensure-package-installed
   'csharp-mode
   'flycheck
   'git-gutter-fringe
   'git-timemachine
   'mic-paren
   'scad-mode
   'smart-compile
   'smartscan
   'yasnippet))

;;;; Emacs window (frame)
(setq frame-title-format (concat "%b@" system-name))  ;%b = buffer name

;;;; Default font
;;; Setting the font here is problematic because it triggers a window
;;; resize, which may push the window off of the screen.
(if (and window-system (eq system-type 'windows-nt))
    (if (string-equal system-name "FDSVBLD01W70027")
        (progn
          ;; Rick, Ashish, and Adam prefer light on dark.
          ;; colorscheme desert
          ;; Rick prefers a larger font.
          (set-face-attribute 'default nil :font "Consolas-13"))
      (set-face-attribute 'default nil :font "Consolas-11")))


;;;; Which function mode
;;; Displays the current function name in the mode line.
(which-function-mode 1)

;;;; Desktop
;;; See http://www.emacswiki.org/emacs/DeskTop
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-base-file-name "desktop")  ;no need for leading dot
(setq desktop-base-lock-name "desktop.lock")  ;no need for leading dot
(setq desktop-path (list emacs-var-directory))
(setq desktop-load-locked-desktop nil)  ;do not load desktop if locked
(add-to-list 'desktop-globals-to-save 'query-replace-history)  ;C-%
(add-to-list 'desktop-globals-to-save 'log-edit-comment-ring)  ;*VC-log*
;;; Clean stale desktop.lock file.
(defun emacs-process-p (pid)
  "If PID is the process ID of an Emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


;;;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (concat emacs-var-directory "emacs.bmk"))
(global-set-key (kbd "<kp-1>") 'bookmark-bmenu-list)
(global-set-key (kbd "<kp-2>") 'bookmark-set)
(global-set-key (kbd "<kp-3>") 'bookmark-jump)


;;;; Yasnippet (templates)
(require 'yasnippet)
(yas-global-mode 1)


;;;; Abbrev-mode
;;; We don't use abbrev-mode explicitly, but elisp/python.el adds
;;; stuff to python-mode-abbrev-table.  And then we are bothered about
;;; saving the modified abbrevs.  So put the abbrev_defs file in var
;;; until we figure it out.
(setq abbrev-file-name (concat emacs-var-directory "abbrev_defs"))


;;;; Web browsing
;;; http://www.emacswiki.org/emacs/emacs-w3m
(when (require 'w3m-load nil t)
  (require 'w3m-load)
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  ;;; Save the w3m buffers between sessions with desktop.el.
  (defun w3m-register-desktop-save ()
    "Set `desktop-save-buffer' to a function returning the current URL."
    (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))
  (add-hook 'w3m-mode-hook 'w3m-register-desktop-save)
  (defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
    "Restore a `w3m' buffer on `desktop' load."
    (when (eq 'w3m-mode desktop-buffer-major-mode)
      (let ((url d-b-misc))
        (when url
          (require 'w3m)
          (if (string-match "^file" url)
              (w3m-find-file (substring url 7))
            (w3m-goto-url-new-session url))
          (current-buffer)))))
  (add-to-list 'desktop-buffer-mode-handlers
               '(w3m-mode . w3m-restore-desktop-buffer)))


;;;; Opening files and buffer manipulation
(defun close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following additions:

* Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.
* Make sure the buffer shown after closing is a user buffer.

A special buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (special-buffer-p is-special-buffer-after)
    (if (string-match "^*" (buffer-name))
        (setq special-buffer-p t)
      (setq special-buffer-p nil))

    ;; Offer to save buffers that are non-empty and modified, even for
    ;; non-file visiting buffer.  Because kill-buffer does not offer
    ;; to save buffers that are not associated with files.
    (when (and (buffer-modified-p)
               (not special-buffer-p)
               (not (string-equal major-mode "dired-mode"))
               (if (equal (buffer-file-name) nil)
                   (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                 t
                 )
               )
      (if (yes-or-no-p
           (concat "Buffer " (buffer-name) " modified; kill anyway? "))
          (save-buffer)
        (set-buffer-modified-p nil)))

    ;; close
    (kill-buffer (current-buffer))

    ;; if emacs buffer, switch to a user buffer
    (if (string-match "^*" (buffer-name))
        (setq is-special-buffer-after t)
      (setq is-special-buffer-after nil))
    (when is-special-buffer-after
      (next-user-buffer))
    ))

(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-user-buffer)
(global-set-key (kbd "<C-tab>") 'next-user-buffer)
(global-set-key (kbd "<kp-subtract>") 'close-current-buffer)

;;; Respawn the scratch buffer if it is killed (C-x k).
(defun kill-scratch-buffer ()
  "Kill the *scratch* buffer and then respawn it."
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))

  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)
(setq initial-scratch-message nil)  ;we know what the scratch buffer is for
(kill-scratch-buffer)  ;install the hook

;;; Enable extra dired functionality such as virtual-dired.
(require 'dired-x)
(setq dired-x-hands-off-my-keys t)  ; don't bind C-x C-f
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))
(add-hook 'dired-mode-hook
          (function (lambda ()
                      (hl-line-mode 1))  ; highlight entire line
                    ))

;;; Enable switching between buffers using substrings.
(iswitchb-mode 1)

;;; Enable menu of recently opened files.
;;; See http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(setq recentf-save-file (concat emacs-var-directory "recentf"))
(recentf-mode 1)
(global-set-key (kbd "<kp-4>") 'recentf-open-files)

;;; Uniquely indentify buffers
;;; See http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; TRAMP remote file access
;;; http://www.gnu.org/software/tramp/
;;; To activate, open file of the form /machine:localname
(require 'tramp)
(require 'tramp-cache)
(require 'tramp-sh)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (concat emacs-var-directory "tramp"))
(if (< emacs-major-version 24) ; broken in emacs-24
    (setq tramp-remote-process-environment
          (split-string
           (replace-regexp-in-string
            "HOME/\.tramp_history"
            "HOME/.var/tramp_history"
            (mapconcat 'identity tramp-remote-process-environment "|"))
           "|")))  ; move ~/.tramp_history file created on target to ~/.var/


;;;; Default to filename at point for C-x C-f.
;;; See http://www.emacswiki.org/emacs/FindFileAtPoint
;;; I tried ido-mode, but I don't like the M-p, M-n behaviour.
(require 'ffap)
(ffap-bindings)
(setq ffap-machine-p-known 'accept)   ; No pinging
(setq ffap-ftp-regexp nil)            ; Disable FTP
(setq ffap-ftp-sans-slash-regexp nil) ; Disable FTP

;;; On UNIX, all strings starting with / are recognized as a path.
;;; This is annoying especially on closing XML tags.
;;; The following advice ignores / as a wrong result.
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  "Advise ffap to ignore files starting with /."
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

;;; Check ffap string for line-number and goto it.
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")
(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))
(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

;;; Search for files in directories other than the current.
;;;
;;; I was using ff-paths for this, but it breaks {svn,git} checkins,
;;; opening files that don't exist yet, TRAMP, and other things I have
;;; already forgotten.
;;;
;;; Add root directories to ffap-c-path in "~/.emacs-local.el":
;;;   (add-to-list 'ffap-c-path "~/src/myproj")
(add-to-list 'ffap-c-path "~/src")
(setq ffap-alist (append ffap-alist '(("\\.py\\'" . ffap-c-mode))))

;;;; Rename buffer and the file it is visiting.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


;;;; Calendar and diary
(require 'calendar)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(calendar-set-date-style 'iso)  ; parse dates in ~/diary
(setq calendar-date-display-form
      '((format "%s-%.2d-%.2d, %s"
                year
                (string-to-number month)
                (string-to-number day)
                dayname)))  ; format displayed dates in diary
(setq diary-number-of-entries 7)  ; number of days to display
(setq diary-list-include-blanks t)  ; include empty days
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)  ; sort entries by time

;;; Encrypted diary handling
(load-library "mydiary")


;;;; Editor behavior

;;; We don't need to see the startup message.
(setq inhibit-startup-message t)

;;; Turn off blinking cursor.
(blink-cursor-mode 0)

;;; Set default major mode to be text-mode instead of fundamental-mode.
;;; Although the doc says that default-major-mode is obsolete since
;;; 23.2 and to use major-mode instead, setting major-mode doesn't
;;; work.
(setq default-major-mode 'text-mode)

;;; Display the size of the buffer, line number, and column number in
;;; the mode line.
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;;; Display line numbers on the left side of the window.
;;; See http://www.emacswiki.org/emacs/LineNumbers
(require 'linum)
(setq linum-format "% 5d")  ;always 5 columns
(global-linum-mode)         ;all buffers

;;; Highlight uncommitted changes.
(when (display-graphic-p)
  (require 'git-gutter-fringe)
  (require 'git-gutter))
(global-git-gutter-mode t)

;;; Flycheck mode.
;;; See https://sourcegraph.com/github.com/robert-zaremba/flycheck

;;; Most checkers have dependencies against external tools that
;;; perform the checking. Use C-c ! ? to see what a checker needs,
;;; e.g. C-c ! ? python-pylint.
;;;
;;; JSON checking requires jsonlint.
;;;   sudo apt-get install nodejs-legacy npm
;;;   sudo npm install jsonlint --global

;;; Enable flycheck mode in all buffers.
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Flycheck key bindings.
(global-set-key (kbd "M-<up>") 'flycheck-previous-error)
(global-set-key (kbd "M-<down>") 'flycheck-next-error)

;;; On-the-fly spell checking.
;;; See http://www.emacswiki.org/emacs/FlySpell
(if (not (eq system-type 'windows-nt))
    (add-hook 'text-mode-hook 'turn-on-flyspell))
(setq ispell-silently-savep t)  ;save dictionary without confirmation

;;; Various key bindings.
(global-set-key (kbd "C-z") 'undo)   ;overrides suspend-frame
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "<kp-7>") (lambda () "" (interactive) (find-file "~/.plan")))
(global-set-key (kbd "<kp-8>") (lambda () (interactive) (diary) (other-window 1)))
(global-set-key (kbd "<kp-9>") 'calendar)
(global-set-key (kbd "C-h n") 'man)  ;overrides view-emacs-news
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))

;;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;;; Delete selected text when typing.
(require 'delsel)  ;required for OpenSUSE-12.1 emacs-23.3-6.1.2
(delete-selection-mode 1)

;;; Vertical motion starting at EOL line keeps to EOL.
(setq track-eol t)

;;; Indentation should insert spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; Display and cleanup bogus whitespace.
;;; See http://www.emacswiki.org/emacs/WhiteSpace
(require 'whitespace)
(setq whitespace-style
      '(face trailing tabs empty indentation space-before-tab))
(global-whitespace-mode 1)
(setq whitespace-action '(auto-cleanup))
(defun whitespace-disable-for-some-files ()
  "Disable whitespace mode for some files."
  (let ((extension (file-name-extension buffer-file-name)))
    (if (or (string-equal extension "sln")
            (string-match "sigrok" buffer-file-name))
        (progn
          (set (make-local-variable 'whitespace-style) '(nil))
          (set (make-local-variable 'whitespace-action) '(nil))
          (set (make-local-variable 'indent-tabs-mode) t)
          ))))
(add-hook 'find-file-hook 'whitespace-disable-for-some-files)

;;; If we do see tabs, they are 4 chars wide.
(setq tab-width 4)

;;; Scroll one line at a time instead of paging.
;;; Paging is what PgUp and PgDn are for.
(setq scroll-conservatively 100)

;;; Remember and restore point location after PgUp,PgDn.
(setq scroll-preserve-screen-position t)

;;; Delete newlines as well as spaces and tabs around point.
(global-set-key (kbd "M-SPC") '(lambda () (interactive) (just-one-space -1)))

;;; Identify variables that are safe to be set as file variables.
(put 'whitespace-line-column 'safe-local-variable 'integerp)

;;; Advanced highlighting of matching parenthesis.
(require 'mic-paren)
(paren-activate)
(setq paren-sexp-mode t)  ; Always highlight the whole s-expression.
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1))))
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (paren-toggle-open-paren-context 1))))

;;; Put all backups in one directory.
;;; See http://www.emacswiki.org/emacs/BackupDirectory
(defvar backup-directory (concat emacs-var-directory "backups/"))
(setq backup-directory-alist `((".*" . ,backup-directory)))

;;; Put all auto-save files in one directory.
;;; See http://www.emacswiki.org/emacs/AutoSave
(defvar auto-save-directory (concat emacs-var-directory "auto-save/"))
(setq auto-save-list-file-prefix auto-save-directory)
(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t)))

;;; When searching forward [Return] ends the search, but puts the
;;; point at the end of the found text.  Define [Ctrl+Return] to put
;;; point at the beginning.
;; http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string (RBEG REND).
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;;; Provide an easy goto-line (^C-g).
(global-set-key (kbd "C-c g") 'goto-line)

;;; Save and restore point (F3, F4).
(define-key global-map (kbd "C-<f3>") '(lambda () (interactive) (point-to-register 33)))  ;^F3 Save
(define-key global-map (kbd "<f3>") '(lambda () (interactive) (jump-to-register 33)))      ;F3 Restore
(define-key global-map (kbd "C-<f4>") '(lambda () (interactive) (point-to-register 34)))  ;^F4 Save
(define-key global-map (kbd "<f4>") '(lambda () (interactive) (jump-to-register 34)))      ;F4 Restore

;;; Move between windows with Shift-arrow keys
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; Use the "clipboard" selection (the one typically is used by
;; C-c/C-v) instead of the X-Windows primary selection (which uses
;; mouse-select/middle-button-click).
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard functions have no
;; effect.  We use xsel instead.  If running under cygwin, we need to
;; modify to use putclip/getclip instead or xsel.
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function ()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))

;;; Insert datetime into current buffer (^C-d).
(defun insert-date ()
  "Insert date time string into current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "C-c d") 'insert-date)

;;; If there is no newline at the end of the file, append one when
;;; saving.  This should not be necessary because most modes should
;;; set require-final-newline to mode-require-final-newline, but most
;;; do not (Emacs-Lisp for one).  The risk here is if we open a binary
;;; file we might append a newline.
(setq require-final-newline t)

;;; Take the URL at point and make it human readable.
(require 'url-humanify)

;;; Jumps between other symbols found at point with M-n and M-p.
;;; From https://github.com/mickeynp/smart-scan
(require 'smartscan)
(global-smartscan-mode 1)


;;;; Printing
;;; See http://www.emacswiki.org/emacs/PrintingFromEmacs
(require 'ps-print)
(setq ps-lpr-command "lp")
(setq ps-number-of-columns 2)
(setq ps-landscape-mode t)
(setq ps-line-number t)
(setq ps-print-color-p nil)
(setq ps-print-header nil)
(setq lpr-command "lp")
(setq lpr-printer-switch "-d ")
(setq lpr-add-switches nil)
(setq lpr-page-header-switches '("-h" "%s" "-F" "--length=61" "--indent=4"))

;;;; Email
;;; Outgoing mail
(require 'smtpmail)
(let* ((computername (downcase system-name))
       (prefixlen (min (length computername) 7))
       (prefix (substring computername 0 prefixlen))
       (realm
        (cond
         ((string-equal prefix "fdsv") "ISC")
         ((string-equal prefix "sps") "ISC")
         ((string-equal computername "apple") "ISC")
         ((string-equal computername "passion") "ISC")
         ((string-equal computername "wampi") "ISC")
         ((string-equal computername "wampi-win2003") "ISC")
         (t "HOME"))))
  (cond
   ((string-equal realm "ISC")
    (setq user-mail-address "tschutter@corelogic.com")
    (setq smtpmail-local-domain "corelogic.com")
    (setq smtpmail-smtp-server "smtp.corelogic.com"))
   (t
    (setq user-mail-address "t.schutter@comcast.net")
    (setq smtpmail-local-domain "schutter.home")
    (setq smtpmail-smtp-server "smtp.schutter.home"))))
;(setq smtpmail-debug-info t)  ;uncomment to debug problems

;;; Use Message to compose mail.
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it)
(add-hook 'message-mode-hook 'turn-on-auto-fill) ;word wrap

;;; ExternalAbook (goobook) integration.
(require 'external-abook)
(custom-set-variables '(external-abook-command
                        (concat
                         emacs-d-directory
                         "bin/goobook-external-abook query '%s'"
                         )))
; Following is not working.
(eval-after-load "message"
  '(progn
     (add-to-list 'message-mode-hook
                  '(lambda ()
                     (local-unset-key "\C-c TAB")
                     (define-key message-mode-map "\C-c TAB" 'external-abook-try-expand
                       )))))


;;;; Eshell
;;; See http://www.emacswiki.org/emacs/CategoryEshell
(require 'eshell)
(setq eshell-directory-name (concat emacs-var-directory "eshell/"))


;;;; ERC InternetRelayChat.
(require 'erc)
(setq erc-nick "tschutter")
(setq erc-prompt-for-password nil)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#sigrok")))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-foolish-content '("^\*\*\* .*: topic set by "
                            "^\*\*\* .* modes: "
                            "^\*\*\* .* was created on"))
(defun erc-foolish-content (msg)
  "Determine if MSG is foolish."
  (erc-list-match erc-foolish-content msg))
(add-hook 'erc-insert-pre-hook
          (lambda (s)
            (when (erc-foolish-content s)
              (setq erc-insert-this nil))))
(require 'erc-log)
(setq erc-log-channels-directory "~/.var/irclog/")
(erc-log-enable)
(require 'easymenu)
(easy-menu-add-item  nil '("tools") ["IRC with ERC" erc t])

;;; BitlBee gateway to IM networks.
;;; sudo apt-get install bitlbee-libpurple
;;; http://emacs-fu.blogspot.com/search/label/erc
;;; http://wiki.bitlbee.org/quickstart
;;; http://wiki.bitlbee.org/bitlbee-sipe
(defun bitlbee-identify ()
  "Generate a message identifying ourself."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify user %s"
                                   (erc-default-target)
                                   bitlbee-password))))
(add-hook 'erc-join-hook 'bitlbee-identify)
(defun chat ()
  "Connect to IM networks using bitlbee."
  (interactive)
  (require 'secrets "secrets.el.gpg")
  (erc :server "localhost" :port 6667 :nick bitlbee-nick))
; register user BITLBEE-PASSWORD
; account add yahoo tom.schutter YAHOO-PASSWORD


;;;; Source code manipulation

;;; Move current line up or down.
(defun move-line-down ()
  "Move current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(defun move-line-up ()
  "Move current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;;; Line wrap regions, function definitions, and function calls.
(defun region-line-wrap ()
  "Line wrap region, breaking at commas."
  (let ((newline (if (eq major-mode (quote vbnet-mode)) " _\n" "\n")))
    (save-excursion
      (save-restriction
        (narrow-to-region (mark) (point))
        (goto-char (point-min))
        (forward-char)
        (if (not (looking-at newline))
            (insert newline))
        (while (re-search-forward "," (point-max) t)
          (if (not (looking-at newline))
              (insert newline)))
        (goto-char (point-max))
        (backward-char)
        (beginning-of-line)
        (if (not (looking-at " *)$"))
            (progn
              (goto-char (point-max))
              (backward-char)
              (insert newline)))))
    (indent-region (mark) (point) nil)))
(defun function-line-wrap ()
  "Line wrap function call or function definition."
  (interactive)
  (let ((original-point (point)))
    (save-excursion
      (mark-defun)
      (let ((defun-begin (point)) (defun-end (mark)))
        ;; Try the sexp that we are inside of.
        (goto-char original-point)
        (backward-up-list)
        (if (looking-at "(")
            (progn
              (set-mark (point))
              (forward-list)
              (region-line-wrap))
          ;; Try the sexp before original-point.
          (goto-char original-point)
          (re-search-backward ")" defun-begin)
          (backward-up-list)
          (set-mark (point))
          (forward-list)
          (region-line-wrap))))))
(define-key global-map (kbd "<f2>") '(lambda () (interactive) (function-line-wrap)))


;;;; Common debugging config.
(gud-tooltip-mode)  ;display a variable's value in a tooltip
;(setq gud-tooltip-echo-area t)  ;use the echo area instead of frames for GUD tooltips


;;;; Compiling.

;;; Set compile command according to mode.
(require 'smart-compile)
(add-to-list 'smart-compile-alist '(cmake-mode . "make -k"))
(add-to-list 'smart-compile-alist '(python-mode . "pycheck %f -s"))

;;; Force a vertical window split.
(defadvice smart-compile (around split-horizontally activate)
  "Split window vertically when smart-compile is called."
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))
(setq compilation-window-height 10)

;;; Bind to F5.
(global-set-key [f5] 'smart-compile)

;;; Globally enable C-n, C-p to cycle through errors.
(defun my-next-error ()
  "Move point to next error and highlight it."
  (interactive)
  (progn
    (next-error)
    (deactivate-mark)
    (end-of-line)
    (activate-mark)
    (beginning-of-line)
    ))
(defun my-previous-error ()
  "Move point to previous error and highlight it."
  (interactive)
  (progn
    (previous-error)
    (deactivate-mark)
    (end-of-line)
    (activate-mark)
    (beginning-of-line)
    ))
(global-set-key (kbd "C-n") 'my-next-error)
(global-set-key (kbd "C-p") 'my-previous-error)


;;;; Python

;;; Static code checks (either ^C-^W or ^C-^V).
(setq py-pychecker-command "pycheck")
(setq python-check-command "pycheck")

;;; Simplify insertion of debugging print statements.
(load "pyp.el")

;;; Python editing.
(add-hook 'python-mode-hook
          (lambda ()
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))  ;on-the-fly spell check in comments
            (make-local-variable 'whitespace-style)
            (add-to-list 'whitespace-style 'lines-tail)  ;highlight cols beyond whitespace-line-column
            (define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)  ;lookup in Python doc
            (define-key python-mode-map (kbd "<f12>") 'pyp)  ;insert debug print
            (define-key python-mode-map "\C-m" 'newline-and-indent)
            ))

;;; Python doc lookup.
;;; See https://github.com/tsgates/pylookup
;; Run "M-x pylookup-update-all" to update database.
(require 'pylookup)
(setq pylookup-program (concat emacs-d-directory "pylookup.py"))  ;executable
(setq pylookup-db-file (concat emacs-var-directory "pylookup.db"))  ;database
(setq pylookup-html-locations '("/usr/share/doc/python2.7/html"))  ;doc source
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

;;; Python ropemacs refactoring.
;;; Currently this is too expensive to do for all Python files, so we
;;; load ropemacs only if requested.
(defun load-ropemacs ()
  "Load pymacs and ropemacs."
  (interactive)
  (require 'pymacs)
  (setq ropemacs-enable-shortcuts nil)
  (pymacs-load "ropemacs" "rope-")
  (define-key ropemacs-local-keymap (kbd "M-/") 'rope-code-assist)
  (define-key ropemacs-local-keymap (kbd "C-c C-d") 'rope-show-doc)
  (define-key ropemacs-local-keymap (kbd "C-c C-g") 'rope-goto-definition)
  (define-key ropemacs-local-keymap (kbd "C-c C-f") 'rope-find-occurrences)
  ;; Automatically save project python buffers before refactorings.
  (setq ropemacs-confirm-saving nil))
(global-set-key "\C-xpl" 'load-ropemacs)


;; CMake
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-default cmake-tab-width 4)
            ))


;;;; C
(defun adjust-indentation-style ()
  "Adjust C indentation style."
  ;; use C-c C-s to determine the syntactic symbol
  ;; use C-h v c-offsets-alist to see current setting for the
  ;; syntactic symbol
  (c-set-offset 'arglist-intro '+)  ; normal indent for first arg
  (c-set-offset 'case-label '+)  ; indent case, not flush w/ switch
  (c-set-offset 'arglist-close '0)  ; no indent for close paren
  )
(add-hook 'c-mode-hook 'adjust-indentation-style)

;;;; C++
(require 'c-includes)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook 'adjust-indentation-style)
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key-after c++-mode-map
              [menu-bar C++ List\ Included\ Files\ Sep]
              '(menu-item "----"))
            (define-key-after c++-mode-map
              [menu-bar C++ List\ Included\ Files]
              '(menu-item "List Included Files" c-includes-current-file))
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))
            (setq-default c-basic-offset 4)
            ))


;;;; C#
;;; See http://www.emacswiki.org/emacs/CSharpMode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook 'adjust-indentation-style)
(add-hook 'csharp-mode-hook
          (lambda ()
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))
            ))


;;;; Java
(add-hook 'java-mode-hook 'adjust-indentation-style)


;;;; reStructuredText documents.
;;; See http://www.emacswiki.org/emacs/reStructuredText
(defun rst-compile-html-preview ()
  "Compile a rst file to html and view in a browser."
  (interactive)
  (let*
      ((bufname (file-name-nondirectory buffer-file-name))
       (basename (file-name-sans-extension bufname))
       (outname (concat temporary-file-directory basename ".html")))
    (set (make-local-variable 'compile-command)
         (concat "rst2html --verbose " bufname " " outname))
    (call-interactively 'compile)
    (browse-url-of-file outname)))
(add-to-list 'smart-compile-alist '(rst-mode rst-compile-html-preview))


;;;; OpenSCAD files.
(autoload 'scad-mode "scad" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))


;;;; VC (version control).
;;; Display warning instead of asking when visiting VC file via simlink.
(setq vc-follow-symlinks nil)

;;; Put list of files in default commit message.
(require 'log-edit)
(setq log-edit-setup-invert t)
(add-hook 'log-edit-hook
          (lambda ()
            (erase-buffer)  ; SETUP inserts stuff we don't want.
            (insert
             (mapconcat 'identity (log-edit-files) ",")
             ": ")))


; WebJump is a programmable Web hotlist (or bookmark) facility that uses Emacs completion
; Jump to a Web site from a programmable hotlist.
(global-set-key [(super j)] 'webjump)
(setq webjump-sites
  '(
    ("Emacs Wiki" .
     [simple-query "www.emacswiki.org"
           "www.emacswiki.org/cgi-bin/wiki/" ""])
    ("Google" .
     [simple-query "www.google.com"
           "www.google.com/search?q=" ""])
    ("Wikipedia" .
     [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
    ("Interactive Weather Information Network" . webjump-to-iwin)
    ))

;;;; Local config.
;;; This *must* be last so that it can override settings in this file.
(if (file-exists-p "~/.emacs-local.el")
    (load-file "~/.emacs-local.el"))

;;; init.el ends here
