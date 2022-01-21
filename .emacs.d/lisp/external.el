;; -*- lexical-binding: t -*-

;; talk to external programs

(require 'use-package)

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
  :straight t
  :defer 2
  :config
  (setq langtool-bin "/usr/bin/languagetool")
  (setq langtool-disabled-rules '("WHITESPACE_RULE"
				  "EN_UNPAIRED_BRACKETS"
				  "COMMA_PARENTHESIS_WHITESPACE"
				  "EN_QUOTES")))

(provide 'csb/external)
