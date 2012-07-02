;;;; emacs(1) config file.

;;;; Configure some standard directory names.
;;; Determine the location of the .emacs.d directory.
(setq emacs-d-directory (file-name-directory load-file-name))
;;; Location for state and cache files.
(setq emacs-var-directory (expand-file-name "~/.var/emacs/"))
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


;;;; Desktop
;;; See http://www.emacswiki.org/emacs/DeskTop
(desktop-save-mode 1)
(setq desktop-base-file-name "desktop")  ;no need for leading dot
(setq desktop-base-lock-name "desktop.lock")  ;no need for leading dot
(setq desktop-path (list emacs-var-directory))
(setq desktop-load-locked-desktop nil)  ;do not load desktop if locked
(add-to-list 'desktop-globals-to-save 'query-replace-history)  ;C-%
(add-to-list 'desktop-globals-to-save 'log-edit-comment-ring)  ;*VC-log*


;;;; Bookmarks
(setq bookmark-default-file (concat emacs-var-directory "emacs.bmk"))
(global-set-key (kbd "<kp-1>") 'bookmark-bmenu-list)
(global-set-key (kbd "<kp-2>") 'bookmark-set)
(global-set-key (kbd "<kp-3>") 'bookmark-jump)


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
  (add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer)))


;;;; Opening files and buffer manipulation
(defun close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following additions:

* Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
* Make sure the buffer shown after closing is a user buffer.

A special buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (special-buffer-p is-special-buffer-after)
    (if (string-match "^*" (buffer-name))
        (setq special-buffer-p t)
      (setq special-buffer-p nil))

    ;; Offer to save buffers that are non-empty and modified, even for non-file visiting buffer.
    ;; Because kill-buffer does not offer to save buffers that are not associated with files.
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
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(global-set-key (kbd "<kp-divide>") 'previous-user-buffer)
(global-set-key (kbd "<kp-multiply>") 'next-user-buffer)
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
(setq dired-x-hands-off-my-keys t)  ; don't bind C-x C-f
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;;; Default to filename at point for C-x C-f.
;;; See http://www.emacswiki.org/emacs/FindFileAtPoint
(require 'ffap)
(ffap-bindings)
(if (featurep 'w3m-load)
    (setq ffap-url-fetcher 'w3m-browse-url)
  (progn
    (setq browse-url-generic-program "/usr/bin/chromium-browser")
    (setq ffap-url-fetcher 'browse-url-generic)))
(setq ffap-machine-p-known 'accept)  ;No pinging
(setq ffap-c-path
      (list
       (getenv "SRC_TREE")
       "~/src/pxpoint"
       "~/src/webservices"
       "/usr/include"
       "/usr/local/include"
       "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
       "C:/Program Files/Java/jdk1.6.0_20/include"))

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
(require 'tramp)  ; required to correctly modify  tramp-remote-process-environment
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (concat emacs-var-directory "tramp"))
(setq tramp-remote-process-environment
      (split-string
       (replace-regexp-in-string
        "HOME/\.tramp_history"
        "HOME/.var/tramp_history"
        (mapconcat 'identity tramp-remote-process-environment "|"))
       "|"))  ; move ~/.tramp_history file created on target to ~/.var/


;;;; Editor behavior

;;; We don't need to see the startup message.
(setq inhibit-startup-message t)

;;; Turn off the toolbar.
(tool-bar-mode -1)

;;; Turn off blinking cursor.
(blink-cursor-mode 0)

;;; Set default major mode to be text-mode instead of fundamental-mode.
;;; Although the doc says that default-major-mode is obsolete since
;;; 23.2 and to use major-mode instead, setting major-mode doesn't
;;; work.
(setq default-major-mode 'text-mode)

;;; Display the size of the buffer, line number, and column number in the mode line.
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)
;(which-function-mode 1)  ;if you can't tell what function you are in, your functions are too long

;;; Display line numbers on the left side of the window.
;;; See http://www.emacswiki.org/emacs/LineNumbers
(require 'linum)
(setq linum-format "% 5d")  ;always 5 columns
(global-linum-mode)         ;all buffers
(defun linum-on ()
  (unless (or
           (minibufferp)    ;except the minibuffer
           (string-match "^\\*\\|&" (buffer-name (current-buffer))))  ;except special buffers
    (linum-mode 1)))

;;; On-the-fly spell checking.
;;; See http://www.emacswiki.org/emacs/FlySpell
(if (not (eq system-type 'windows-nt))
    (add-hook 'text-mode-hook 'turn-on-flyspell))
(setq ispell-silently-savep t)  ;save the personal dictionary without confirmation

;;; Various key bindings.
(global-set-key (kbd "C-z") 'undo)   ;overrides suspend-frame
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "<kp-7>") (lambda () "" (interactive) (find-file "~/.plan")))
(global-set-key (kbd "C-h n") 'man)  ;overrides view-emacs-news

;;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;;; Delete selected text when typing.
(require 'delsel)  ;required for OpenSUSE-12.1 emacs-23.3-6.1.2
(delete-selection-mode 1)

;;; Vertical motion starting at EOL line keeps to EOL.
(setq track-eol t)

;;; Indentation should insert spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; If we do see tabs, they are 4 chars wide.
(setq default-tab-width 4)

;;; Scroll one line at a time instead of paging.
;;; Paging is what PgUp and PgDn are for.
(setq scroll-conservatively 100)

;;; Remember and restore point location after PgUp,PgDn
(setq scroll-preserve-screen-position t)

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

;;; Enable switching between buffers using substrings.
(iswitchb-mode 1)

;;; Put all backups in one directory.
;;; See http://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory (concat emacs-var-directory "backups/"))
(setq backup-directory-alist `(("." . ,backup-directory)))
(defun make-backup-file-name (file)
  (concat backup-directory (file-name-nondirectory file) "~"))
(setq auto-save-list-file-prefix backup-directory)

;;; Provide an easy goto-line (^C-g).
(global-set-key (kbd "C-c g") 'goto-line)

;;; Save and restore point (F3, F4).
(define-key global-map (kbd "C-<f3>") '(lambda () (interactive) (point-to-register 33)))  ;^F3 Save
(define-key global-map (kbd "<f3>") '(lambda () (interactive) (jump-to-register 33)))      ;F3 Restore
(define-key global-map (kbd "C-<f4>") '(lambda () (interactive) (point-to-register 34)))  ;^F4 Save
(define-key global-map (kbd "<f4>") '(lambda () (interactive) (jump-to-register 34)))      ;F4 Restore

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
  "Inserts date time string into current buffer."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "C-c d") 'insert-date)

;;; If there is no newline at the end of the file, append one when
;;; saving.  This should not be necessary because most modes should
;;; set require-final-newline to mode-require-final-newline, but most
;;; do not (Emacs-Lisp for one).  The risk here is if we open a binary
;;; file we might append a newline.
(setq require-final-newline t)


;;;; Printing
;;; See http://www.emacswiki.org/emacs/PrintingFromEmacs
(require 'ps-print)
(setq ps-number-of-columns 2)
(setq ps-landscape-mode t)
(setq ps-line-number t)
(setq ps-print-color-p nil)
(setq ps-print-header nil)
(setq lpr-page-header-switches '("-F" "--length=61" "--indent=4"))


;;;; Email
;;; Outgoing mail
(require 'smtpmail)
(let* ((computername (downcase system-name))
       (prefixlen (min (length computername) 7))
       (prefix (substring computername 0 prefixlen))
       (realm
        (cond
         ((string-equal prefix "fdsvbld") "ISC")
         ((string-equal prefix "fdsvdfw") "ISCP")
         ((string-equal prefix "fdsvmad") "ISC")
         ((string-equal prefix "fdsvsna") "ISCP")
         ((string-equal computername "apple") "ISC")
         ((string-equal computername "passion") "ISC")
         ((string-equal computername "wampi") "ISC")
         ((string-equal computername "wampi-win2003") "ISC")
         (t "HOME"))))
  (cond
   ((string-equal realm "ISCP")
    (setq user-mail-address "tschutter@corelogic.com")
    (setq smtpmail-local-domain "corelogic.com")
    (setq smtpmail-smtp-server "smtp.corelogic.com"))
   ((string-equal realm "ISC")
    (setq user-mail-address "tschutter@corelogic.com")
    (setq smtpmail-local-domain "corelogic.com")
    (setq smtpmail-smtp-server "fdsvdfw01vxms01.infosolco.com"))
   (t
    (setq user-mail-address "t.schutter@comcast.net")
    (setq smtpmail-local-domain "schutter.home")
    (setq smtpmail-smtp-server "smtp.schutter.home"))))
;(setq smtpmail-debug-info t)  ;uncomment to debug problems

;;; Use Message to compose mail.
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it)
(add-hook 'message-mode-hook 'turn-on-auto-fill) ;word wrap

;;; LBDB (abook) integration.
(autoload 'lbdb "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-region "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-maybe-region "lbdb" "Query the Little Brother's Database" t)
(add-hook 'message-setup-hook
          (lambda ()
            (require 'lbdb-complete)
            (define-key message-mode-map (kbd "C-c TAB") 'lbdb-complete)
            ))


;;;; Eshell
;;; See http://www.emacswiki.org/emacs/CategoryEshell
(require 'eshell)
(setq eshell-directory-name (concat emacs-var-directory "eshell/"))


;;;; BitlBee ERC InternetRelayChat.
;;; sudo apt-get install bitlbee-libpurple
;;; http://emacs-fu.blogspot.com/search/label/erc
;;; http://wiki.bitlbee.org/quickstart
;;; http://wiki.bitlbee.org/bitlbee-sipe
(defun bitlbee-identify ()
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

;;; Display and cleanup bogus whitespace.
;;; See http://www.emacswiki.org/emacs/WhiteSpace
(require 'whitespace)
(setq whitespace-style '(face trailing tabs empty indentation space-before-tab))
(global-whitespace-mode 1)
(setq whitespace-action '(auto-cleanup))

;;; Line wrap regions, function definitions, and function calls.
(defun region-line-wrap ()
  "Line wrap region, breaking at commas"
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
  "Line wrap function call or function definition"
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


;;;; Python

;;; Static code checks (either ^C-^W or ^C-^V).
(setq py-pychecker-command "pycheck")
(setq python-check-command "pycheck")

;;; Simplify insertion of debugging print statements.
(load "pyp.el")
(defun pdb-insert-break ()
  "Insert the code necessary to drop into pdb at the current point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "import pdb; pdb.set_trace()")
    (newline)
    (beginning-of-line 0)
    (indent-for-tab-command)
    ))

;;; Python editing.
(add-hook 'python-mode-hook
          (lambda ()
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))  ;on-the-fly spell check in comments
            (make-local-variable 'whitespace-style)
            (add-to-list 'whitespace-style 'lines-tail)
            (define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)  ;lookup in Python doc
            (define-key python-mode-map (kbd "<f11>") 'pdb-insert-break)  ;insert debug break
            (define-key python-mode-map (kbd "<f12>") 'pyp)  ;insert debug print
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
  "Load pymacs and ropemacs"
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


;;;; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda ()
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))
            (setq-default c-basic-offset 4)
            ))


;;;; C#
;;; See http://www.emacswiki.org/emacs/CSharpMode
;;; Downloaded 0.8.5 from http://code.google.com/p/csharpmode/
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook
          (lambda ()
            (if (not (eq system-type 'windows-nt))
                (flyspell-prog-mode))
            ))


;;;; reStructuredText documents.
;;; See http://www.emacswiki.org/emacs/reStructuredText
;;; Ubuntu drops the .py extensions on the rst programs in python-docutils.
(setq rst-compile-toolsets
  '((html . ("rst2html" ".html" nil))
    (latex . ("rst2latex" ".tex" nil))
    (newlatex . ("rst2newlatex" ".tex" nil))
    (pseudoxml . ("rst2pseudoxml" ".xml" nil))
    (xml . ("rst2xml" ".xml" nil))))


;;;; OpenSCAD files.
;;; Downloaded v88 from https://github.com/openscad/openscad/blob/master/contrib/scad.el
(autoload 'scad-mode "scad" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))


;;;; Local config.
;;; This *must* be last so that it can override settings here.
(if (file-exists-p "~/.emacs-local.el")
    (load-file "~/.emacs-local.el"))
