;; Chris Burroughs
; General Custom keybindings

; in case of old emacs versions
(global-set-key "\M-g" 'goto-line)

;; Item 2: Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Item 9: Master Emacs's regular expressions
(defalias 'qrr 'query-replace-regexp)

; Need to force this some times?
(global-set-key "\C-c\C-c" 'comment-region) 

; cua-like
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

; heresy; like the keybindings of the beas
(global-set-key "\M-j" 'previous-line)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-h" 'backward-char)
(global-set-key "\M-l" 'forward-char)


; For when I get confused
(global-set-key "\M-p" 'previous-line)
(global-set-key "\M-n" 'next-line)