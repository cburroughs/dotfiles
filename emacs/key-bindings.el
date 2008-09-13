;; Chris Burroughs
; General Custom keybindings

; in case of old emacs versions
(global-set-key "\M-g" 'goto-line)

;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)