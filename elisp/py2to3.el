;;
;; Convert Python script from python2 to python3.
;;

;;; Code:

#!/usr/bin/env python

(defun fixup-shebang ()
  "python -> python3"
  (goto-char (point-min))
  (while (re-search-forward "#! */usr/bin/env python\n" (point-max) t)
    (replace-match "#!/usr/bin/env python3\n"))
  )

(defun fixup-argparse ()
  "optparse -> argparse"
  (goto-char (point-min))
  ; import
  (while (re-search-forward "\nimport optparse\n" (point-max) t)
    (replace-match "\nimport argparse\n"))
  ; create parser
  (while (re-search-forward "option_parser = optparse\.OptionParser" (point-max) t)
    (replace-match "arg_parser = argparse.ArgumentParser"))
  (while (re-search-forward "(usage=\"usage:.*\n *)\" *" (point-max) t)
    (replace-match "#\1description=\""))
  (while (re-search-forward "option_parser\.add_option" (point-max) t)
    (replace-match "arg_parser.add_argument"))
  (while (re-search-forward "(options, args) = option_parser\.parse_args" (point-max) t)
    (replace-match "args = arg_parser.parse_args"))
  (while (re-search-forward "options\." (point-max) t)
    (replace-match "args."))
  )

(defun py2to3 ()
  "Apply all fixups."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fixup-shebang)
      (fixup-argparse)
      )))
