;; talk to external programs

;; keep things clean!

(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)

;; use mzscheme, obviously it must be installed
(setq scheme-program-name "mzscheme")

(defun p-markdown->html (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f markdown -t html"  nil 't))

(defun p-html->markdown (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f html -t markdown"  nil 't))
