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
;; select a color theme and highlight current line if in not in term
(if window-system
    (progn
      (require 'color-theme)
      (when (my-gentoo?)
        (color-theme-initialize)) ; gentoo wants, ubuntu hates
      (color-theme-clarity)
      (global-hl-line-mode 1)
      (set-face-background 'hl-line "gray18")))

; awesome fonts only with emacs 23!
; TODO: What if the system does not have this font?  
(if (string-match "23\." (emacs-version))
    (if (my-gentoo?)
        (set-frame-font "DejaVu Sans Mono-10")
      (set-frame-font "Bitstream Vera Sans Mono-10")))


;; Start nice and tall, but should still be 80 char wide
(setq initial-frame-alist '((top . 1) (height . 63)))


;; http://emacs-fu.blogspot.com/2008/12/zooming-inout.html
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 

(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))


(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
fewer than 80 columns."
                                        ; From http://hjiang.net/archives/253
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
80 columns."
    (if (> (window-width w) (* 2 81))
        (let ((w2 (split-window w 82 t)))
          (smart-split-helper w2))))
  (smart-split-helper nil))


