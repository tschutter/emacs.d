;;;; init.el --- emacs(1) config file

;;; Turn off tool bar early in startup to avoid momentary display.
(when window-system
  (tool-bar-mode -1))

;;; Setup package.
(require 'package)

;; Milkypostman’s Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Activate installed packages.
(package-initialize)

;;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Reduce use-package load time.
;; From the end of https://github.com/jwiegley/use-package/blob/master/README.md
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ; if you use :diminish
(require 'bind-key)                ; if you use any :bind variant

;; Uncomment to debug use-package.
;(setq use-package-verbose t)

;;; Load the config.
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;; Load the local config.
;; This *must* be last so that it can override settings in config.org.
(if (file-exists-p "~/.emacs-local.el")
    (load-file "~/.emacs-local.el"))

;;; init.el ends here
