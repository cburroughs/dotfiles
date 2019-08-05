;; Cosmetic changes

;; Disable the tool bar; finally a cool kid?
(tool-bar-mode -1)

(if (not window-system)
    (menu-bar-mode -1))


(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t) ;; not sure of effect

;; boo splash screens
(setq inhibit-startup-message t)

;; defaults to showing parens
(show-paren-mode t)

(set-scroll-bar-mode 'right)

; try to have a decent name
(setq frame-title-format '(buffer-file-name "%b -- %f" ( "%b" ) ) )


;; Default to highlighting
(global-hl-line-mode 1)
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2011-12/msg00135.html
(defun visual-line-line-range ()
  (save-excursion
    (cons (progn (vertical-motion 0) (point))
         (progn (vertical-motion 1) (+ (point) 0)))))

(setq hl-line-range-function 'visual-line-line-range)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")

;; NOTE That while it *almost* works, custom themes can not reliably be reset, or
;; switched between
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=15687
;; https://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
;; https://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters

(defun dark-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (setq org-todo-keyword-faces
        '(("TODO" . "#C16069")
          ("WAITING" . "#dc752f")
          ("DONE" . "#98be65")))
  (load-theme 'claritas t))

(defun light-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (seq org-todo-keyword-faces nil)
  (setq leuven-scale-outline-headlines nil)
  (setq leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

;; https://emacs.stackexchange.com/questions/7151/is-there-a-way-to-detect-that-emacs-is-running-in-a-terminal
(if (display-graphic-p)
    (dark-theme)
  (progn
    (global-hl-line-mode 0)
    (load-theme 'wheatgrass t)))


; TODO: What if the system does not have this font?
(set-frame-font "DejaVu Sans Mono-11")

;; Start nice and tall, but should still be 80 char wide
;; Note that the height is hard coded and depends on the resolution
(if window-system
  (setq initial-frame-alist '((width . 80) (height . 55))))

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
  (olivetti-mode))


;; Dependency of doom-modeline
(use-package all-the-icons
  :ensure t
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
  :ensure t
  :pin melpa-stable
  :config
  (setq  minions-mode-line-lighter
         (all-the-icons-octicon  "tools"
                                 :height 1.0 :v-adjust .05))
  (minions-mode 1))



;; Alternative tabs
(use-package centaur-tabs
  :ensure t
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

;; https://github.com/ema2159/centaur-tabs/issues/34
(eval-after-load 'centaur-tabs-mode
  (setq uniquify-buffer-name-style 'forward))


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


(use-package dashboard
  :ensure t
  :pin melpa-stable
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))
