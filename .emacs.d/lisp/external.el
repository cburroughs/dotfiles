;; talk to external programs

;; use mzscheme, obviously it must be installed
(setq scheme-program-name "mzscheme")

(defun p-markdown->html (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f markdown -t html"  nil 't))

(defun p-html->markdown (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f html -t markdown"  nil 't))
