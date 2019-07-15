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
  (setq initial-frame-alist '((width . 80) (height . 52))))

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


;; full screen margins: Olivetti is a simple Emacs minor mode for a nice writing
;; environment.
(use-package olivetti
  :ensure t
  :pin melpa-stable)

(defun prose-time ()
  "Full screen writing focus"
  (interactive)
  (global-hl-line-mode 0)
  (olivetti-mode))


;; Dependency of doom-modeline
(use-package all-the-icons
  :pin melpa-stable)

;; Fancy modelines
(use-package doom-modeline
  :ensure t
  :pin melpa-stable
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 22)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-minor-modes t)
  (set-face-attribute 'mode-line nil :height 105)
  (set-face-attribute 'mode-line-inactive nil :height 105))


;; Creates a "draw" of sorts for minor modes
;; https://manuel-uberti.github.io/emacs/2018/03/10/moody-and-minions/
(use-package minions
  :pin melpa-stable
  :config
  (setq  minions-mode-line-lighter
         (all-the-icons-octicon  "tools"
                                 :height 1.0 :v-adjust .05))
  (minions-mode 1))



;; Alternative tabs
(use-package centaur-tabs
  :demand
  :bind
  ;; TODO: Switch to only using terminal style PageUp/Down for fewer conflicts?
  ([(C-S-iso-lefttab)] . centaur-tabs-backward)
  ([(control tab)] . centaur-tabs-forward)
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :config
;;  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-set-bar 'over) ;; Theme dependent?
  (setq centaur-tabs-style "box")  ;; Theme dependent?
  (defun centaur-tabs-hide-tab (x)
     (let ((name (format "%s" x)))
       (or
        (string-prefix-p "*" name)
        (string-prefix-p "COMMIT_EDITMSG" name)
        (and (string-prefix-p "magit" name)
             (not (file-name-extension name)))
	  )))
  ;;  (centaur-tabs-inherit-tabbar-faces)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))


;; TODO: Choose
;; tabbar is pretty awesome
(require 'tabbar)
;; (tabbar-mode t)
;; (global-set-key [(C-S-iso-lefttab)] 'tabbar-backward) ; why funy binding?
;; (global-set-key [(control tab)]       'tabbar-forward)
(set-face-attribute
 'tabbar-default-face nil
 :background "gray60")
(set-face-attribute
 'tabbar-unselected-face nil
 :background "gray85"
 :foreground "gray30"
 :box nil)
(set-face-attribute
 'tabbar-selected-face nil
 :background "#f2f2f6"
 :foreground "black"
 :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
 :height 0.7)

;; Don't cycle *scratch* and friends
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (find (aref (buffer-name buffer) 0) " *"))
         (buffer-list))))
