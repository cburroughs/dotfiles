;; misc changes and variable settings

(use-package esup
  :ensure t
  :defer 1)


;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; copy-paste should work with other X clients
;; duh; only do this when you have x
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; change spelling
(setq-default ispell-program-name "aspell")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; graphviz dot mode requires this to be set, unclear how it is different from the above
(setq-default default-tab-width 4)

;; prefer hippie-expand over deavrez
(global-set-key (kbd "M-/") 'hippie-expand)

;; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev try-expand-dabbrev-all-buffers 
                           try-expand-dabbrev-from-kill
                           try-complete-file-name-partially 
                           try-complete-file-name
                           try-expand-all-abbrevs try-expand-list 
                           try-expand-line
                           try-complete-lisp-symbol-partially
                           try-complete-lisp-symbol))

;; Try to avoid giving buffers dumb names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; find files recursivly
(require 'find-recursive)

;; default to firefox in new tab
(setq browse-url-default-browser 'browse-url-firefox-program)
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab 't) ; why does this not work?

(require 'anything)
(require 'anything-find-project-resources)
(global-set-key (kbd "M-r") 'anything-find-resource)
(add-to-list `anything-find-resource--project-root-files '"build.sbt")
(add-to-list `anything-find-resource--project-root-files '"pants.ini")
(add-to-list `anything-find-resource--ignore-dirs '"dist/")
(add-to-list `anything-find-resource--ignore-dirs '"target/")
(add-to-list `anything-find-resource--ignore-files '"*.pyc")

(autoload 'columnize-text "columnize"
  "Formats a list of items into columns (pillars)" t)

;; http://www.emacswiki.org/emacs/FillParagraph
;; http://jblevins.org/projects/markdown-mode/
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.\\|[ \t]*: ")


(use-package lorem-ipsum
             :ensure t)

(use-package neotree
  :ensure t
  :pin melpa-stable
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-autorefresh nil))


;; http://pragmaticemacs.com/emacs/world-clock-in-emacs/
;; for display-time-world
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Los_Angeles")
        ("America/New_York" "New York")
        ("America/Vancouver" "Vancouver")
        ("Europe/Berlin" "Berlin")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Asia/Seoul" "Seoul")
        ("Asia/Shanghai" "Bejing/Shanghai")
        ("Asia/Tokyo" "Tokyo")))