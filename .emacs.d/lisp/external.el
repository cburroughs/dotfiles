;; talk to external programs

;; use mzscheme, obviously it must be installed
(setq scheme-program-name "mzscheme")

(defun p-markdown->html (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f markdown -t html"  nil 't))

(defun p-html->markdown (beg end)
  (interactive "r")
  (shell-command-on-region beg end "pandoc -f html -t markdown"  nil 't))


;; langtool grammar check
(use-package langtool
  :ensure t
  :pin melpa
  :defer 2
  :config
  (setq langtool-bin "/usr/bin/languagetool")
  (setq langtool-disabled-rules '("WHITESPACE_RULE"
				  "EN_UNPAIRED_BRACKETS"
				  "COMMA_PARENTHESIS_WHITESPACE"
				  "EN_QUOTES")))
