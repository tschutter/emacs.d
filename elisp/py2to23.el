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

(defun replace-all (from-regexp to-string)
  "Replace all FROM-REGEXP to TO-STRING in buffer."
  (goto-char (point-min))
  (while (re-search-forward from-regexp nil t)
    (replace-match to-string)))

(defun convert-shebang-to-py23 ()
  "Change shebang line with explicit 'python2' to generic 'python'."
  (replace-all
   "^#! */usr/bin/env python2$"
   "#!/usr/bin/env python"))

(defun convert-shebang-to-py3 ()
  "Change shebang line with explicit 'python2' to explicit 'python3'."
  (replace-all
   "^#! */usr/bin/env python2?$"
   "#!/usr/bin/env python3"))

(defun remove-future-imports ()
  "Remove any imports from the future."
  (replace-all
   "^from __future__ import [^\n]+\n"
   ""))

(defun convert-argparse ()
  "Convert from optparse to argparse."
  (interactive)

  ;; convert import
  (replace-all
   "^\\(import optparse\\|from optparse import OptionParser\\)$"
   "import argparse")

  ;; convert parser creation
  (replace-all
   "\\(option_parser\\|optionParser\\) = \\(optparse\\.\\)?OptionParser"
   "arg_parser = argparse.ArgumentParser")
  (replace-all
   "(usage=\"usage:.*\n *)\" *"
   "# FIXME \\1description=\"")
  (replace-all
   "usage=($"
   "description=(  # FIXME")

  ;; convert addition of arguments to parser
  (replace-all
   "\\(option_parser\\|optionParser\\)\\.add_option"
   "arg_parser.add_argument")

  ;; convert use of "type" parameter of add_argument()
  (replace-all
   " type=\"\\([^\"]+\\)\","
   " type=\\1,")

  ;; convert use of "%" operator
  (replace-all "%default" "%(default)s")

  ;; convert call to parse_args()
  (replace-all
   "(options, args) = \\(option_parser\\|optionParser\\)\\.parse_args"
   "args = arg_parser.parse_args")

  ;; convert general use of option_parser object
  (replace-all
   "\\(option_parser\\|optionParser\\)\\."
   "arg_parser.")

  ;; convert general use of options object returned by parse_args()
  (replace-all "options," "args,")
  (replace-all "options\\." "args.")
  (replace-all "(options)" "(args)"))

(defun py2-to-py23 ()
  "Convert Python script to run under python2 or python3."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (convert-shebang-to-py23)
      (convert-argparse))))

(defun py2-to-py3 ()
  "Convert Python script to run under python3."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (convert-shebang-to-py3)
      (remove-future-imports)
      (convert-argparse)
      (replace-all "\\.iterkeys(" ".keys(")
      (replace-all "\\.itervalues(" ".values(")
      (replace-all "\\.iteritems(" ".items("))))

(provide 'py2to23)
;;; py2to23.el ends here
