;; Chris Burroughs
; General Custom keybindings

; in case of old emacs versions
(global-set-key "\M-g" 'goto-line)

;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; Need to force this some times
(global-set-key "\C-c\C-c" 'comment-region) 

; cua-like
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

; heresy; like the keybindings of the beast (well just enough to be confusing)
;  up down <- ->
(global-set-key "\M-j" 'previous-line)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-l" 'backward-char)
(global-set-key "\M-;" 'forward-char)
