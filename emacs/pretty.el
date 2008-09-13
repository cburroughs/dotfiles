;; Cosmetic changes

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t) ;; not sure of effect

;; boo splash screens
(setq inhibit-startup-message t)

;; defaults to showing parens
(show-paren-mode t)

(set-scroll-bar-mode 'right)

; try to have a decent name
(setq frame-title-format '(buffer-file-name "%b -- %f" ( "%b" ) ) )

;app-emacs/color-theme
; select a color theme and highlight current line if in not in term
(if window-system
    (progn
      (require 'color-theme)
      (when (my-gentoo?)
        (color-theme-initialize)) ; gentoo wants, ubuntu hates
      (color-theme-clarity)
      (global-hl-line-mode 1)
      (set-face-background 'hl-line "gray18")))

; awesome fonts only with emacs 23!
; TODO: What if the system does not have this font?  What about Deaja vu?
(if (string-match "23\." (emacs-version))
    (set-default-font "Bitstream Vera Sans Mono-10"))

