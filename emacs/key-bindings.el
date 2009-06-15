;; Chris Burroughs
;; General Custom keybindings

;; in case of old emacs versions
(global-set-key "\M-g" 'goto-line)

;; Item 2: Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Item 9: Master Emacs's regular expressions
(defalias 'qrr 'query-replace-regexp)

;; Need to force this some times?
(global-set-key "\C-c\C-c" 'comment-region) 

;; cua-like
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)


;; vi/egro
(require 'ergo-movement-mode)
(ergo-movement-mode 1)


;; For when I get confused
(global-set-key "\M-p" 'previous-line)
(global-set-key "\M-n" 'next-line)

;; spell checking
(global-set-key [f7] 'ispell-buffer)
(global-set-key [Shift-f7] 'ispell-comments-and-strings)

;; hey the keyboard does say open on that key
(global-set-key [f5] 'browse-url-at-point)

;; move between buffers with meta --> arrow
(require 'windmove)
(windmove-default-keybindings 'meta)

;; esk: Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; esk: Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

