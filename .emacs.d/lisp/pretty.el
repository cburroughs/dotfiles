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


;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")

(defun dark-theme ()
  (interactive)
  (load-theme 'clarity t)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "gray18"))


(defun light-theme ()
  (interactive)
  (load-theme 'leuven t)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "snow"))

(if window-system
    (progn
      (dark-theme)))

; TODO: What if the system does not have this font?  
(set-frame-font "DejaVu Sans Mono-11")

;; Start nice and tall, but should still be 80 char wide
(if window-system
  (setq initial-frame-alist '((top . 0) (width . 80) (height . 52))))

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


;; esk:  Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))



;; fill, width, writing

(setq-default fill-column 80)

;; http://emacshorrors.com/posts/longlines-mode.html
(use-package visual-fill-column
             :ensure t
             :pin melpa-stable
             :init
             (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
