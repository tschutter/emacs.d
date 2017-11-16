;;; python2to23 --- Convert Python script to run under python2 or python3.

;; Copyright (C) 2014 Tom Schutter

;; Author: Tom Schutter <t.schutter@comcast.net>
;; Created: 2013-12-01
;; Version: 1.0
;; Keywords: python languages

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; None yet.

;;; Code:

(defun fixup-shebang ()
  "Change shebang line with explicit 'python2' to generic 'python'."
  (goto-char (point-min))
  (while (re-search-forward "#! */usr/bin/env python2\n" (point-max) t)
    (replace-match "#!/usr/bin/env python\n")))

(defun fixup-argparse ()
  "Convert from optparse to argparse."
  (interactive)
  (goto-char (point-min))

  ;; fix import
  (while (re-search-forward "\nimport optparse\n" (point-max) t)
    (replace-match "\nimport argparse\n"))
  (while (re-search-forward "\nfrom optparse import OptionParser\n" (point-max) t)
    (replace-match "\nimport argparse\n"))

  ;; fix parser creation
  (while (re-search-forward "option_parser = optparse\.OptionParser" (point-max) t)
    (replace-match "arg_parser = argparse.ArgumentParser"))
  (while (re-search-forward "optionParser = OptionParser" (point-max) t)
    (replace-match "arg_parser = argparse.ArgumentParser"))
  (while (re-search-forward "(usage=\"usage:.*\n *)\" *" (point-max) t)
    (replace-match "#\1description=\""))

  ;; fix addition of arguments to parser
  (while (re-search-forward "option_parser\.add_option" (point-max) t)
    (replace-match "arg_parser.add_argument"))
  (while (re-search-forward "optionParser\.add_option" (point-max) t)
    (replace-match "arg_parser.add_argument"))
  (while (re-search-forward "(options, args) = option_parser\.parse_args" (point-max) t)
    (replace-match "args = arg_parser.parse_args"))
  (while (re-search-forward "(options, args) = optionParser\.parse_args" (point-max) t)
    (replace-match "args = arg_parser.parse_args"))

  ;; fix general use of option_parser object
  (while (re-search-forward "option_parser\\." (point-max) t)
    (replace-match "arg_parser."))
  (while (re-search-forward "optionParser\\." (point-max) t)
    (replace-match "arg_parser."))

  ;; fix default printing in help
  (while (re-search-forward "default=%default" (point-max) t)
    (replace-match "default=%(default)s"))

  ;; fix general use of options object
  (while (re-search-forward "options\\." (point-max) t)
    (replace-match "args.")))

(defun py2to23 ()
  "Convert Python script to run under python2 or python3."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fixup-shebang)
      (fixup-argparse))))

(provide 'py2to23)
;;; py2to23.el ends here
