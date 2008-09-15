; misc changes and variable settings

;make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; copy-paste should work with other X clients
; duh; only do this when you have x
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; change spelling
(setq-default ispell-program-name "aspell")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; prefer hippie-expand over deavrez
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

; Try to avoid giving buffers dumb names
(require 'uniquify)
