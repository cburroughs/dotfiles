; misc changes and variable settings

;make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)


;; copy-paste should work with other X clients
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; change spelling
(setq-default ispell-program-name "aspell")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)



