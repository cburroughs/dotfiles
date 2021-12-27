;; -*- lexical-binding: t -*-

;; Cosmetic changes

;; I don't blink on the terminal, let's try being consistent
(blink-cursor-mode -1)

;; Disable the tool bar; finally a cool kid?
(tool-bar-mode -1)

(if (not window-system)
    (menu-bar-mode -1))


(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t) ;; not sure of effect

;; boo splash screens
(setq inhibit-startup-message t)

;; defaults to showing parens
(setq show-paren-delay 0)
(show-paren-mode t)

;; 💓 Clearlooks gtk2 forever
(push '(vertical-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
;; (set-scroll-bar-mode 'right)

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
        `(("TODO" . ,palette/doom-one/red)
          ("WAITING" . ,palette/doom-one/orange)
          ("DONE" . ,palette/doom-one/green)
          ("CANCELED" . ,palette/doom-one/base6)))
  (load-theme 'claritas t))

(defun light-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (setq org-todo-keyword-faces nil)
  (setq leuven-scale-outline-headlines nil)
  (setq leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))


(use-package spacemacs-theme
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :defer t)

(use-package monokai-theme
  :straight t
  :defer t)

(use-package dracula-theme
  :straight t
  :defer t)

(use-package zenburn-theme
  :straight t
  :defer t)

(use-package solarized-theme
  :straight t
  :defer t)


(defun space-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (setq org-todo-keyword-faces nil)
  (load-theme 'spacemacs-dark t))

(defun my-doom-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (setq org-todo-keyword-faces nil)
  (load-theme 'doom-one t))


;; https://emacs.stackexchange.com/questions/7151/is-there-a-way-to-detect-that-emacs-is-running-in-a-terminal
(if (display-graphic-p)
    (dark-theme)
  (progn
    (global-hl-line-mode 0)
    (load-theme 'wheatgrass t)))


; TODO: What if the system does not have this font?
(set-frame-font "DejaVu Sans Mono-11")
(set-face-font 'default "DejaVu Sans Mono-11")
;;(set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-11")
;; https://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
;; Maybe also try symbola?
(set-fontset-font "fontset-default" nil
                  (font-spec :size 11 :name "Symbola"))

;; Start nice and tall, but should still be 80 char wide
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(if window-system
    (add-to-list 'default-frame-alist '(fullscreen . fullheight)))


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


;; fill, width, writing
(setq-default fill-column 78)

;; http://emacshorrors.com/posts/longlines-mode.html
(use-package visual-fill-column
  :straight t
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))


;; full screen margins: Olivetti is a simple Emacs minor mode for a nice writing
;; environment.
(use-package olivetti
  :straight t
  :defer 1)

(defun prose-time ()
  "Full screen writing focus"
  (interactive)
  (olivetti-mode))


;; Dependency of doom-modeline; and kind of cool
(use-package all-the-icons
  :straight t)


;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
(defun csb/align-mode-line (left right &optional resered-right-width)
  "Return a string of `window-width' length containing LEFT, and
RIGHT aligned respectively."
  (let* ((align-mode-line-width ; how much is avilable for us to align
          (if resered-right-width
              (- (window-total-width) resered-right-width)
            (window-total-width)))
         (available-width
          (- align-mode-line-width
             (+ (length (format-mode-line left))
                (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right
            '(mode-line-end-spaces))))

;; https://github.com/jdtsmith/mlscroll/issues/11
(defun csb/mlscroll-mode-right-reserved ()
    "How much does mlscroll mode need for the magic on the right hand side?"
  (if (and (bound-and-true-p mlscroll-mode)
           (bound-and-true-p mlscroll-width-chars)
           (not (bound-and-true-p mlscroll-alter-percent-position)))
      ;; magic guess padding for mode-line-end-spaces
      (+ mlscroll-width-chars 2)
         2))


;; adapted from  doom-modeline-update-buffer-file-state-icon 
(defun csb/mode-line-buffer-file-state-icon  ()
  (ignore-errors
    (cond (buffer-read-only
           (propertize "🔒" 'face '(:height 1.1)))
          ((and buffer-file-name (buffer-modified-p))
           (propertize "💾" 'face '(:height 1.1)))
           ((and buffer-file-name
                 (not (file-exists-p buffer-file-name)))
            (propertize "🚫"  'face  '(:foreground "Pink" :height 1.1)))
           ('t "-"))))


(defun csb/lsp-state-icon ()
  (ignore-errors
    (if (bound-and-true-p lsp-mode)
        (if (lsp-workspaces)
            (propertize "🚀" 'face '(:height 1.1))
            (propertize "💥" 'face '(:height 1.1)))
            "-")))



;; Keep `doom-modeline-current-window' up-to-date
(defun csb/modeline-get-current-window (&optional frame)
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar csb/modeline-current-window (csb/modeline-get-current-window))

;; From https://github.com/seagle0128/doom-modeline/blob/dc93cdec543e25022db7b034af49d57b6ee1c289/doom-modeline-core.el#L840
(defun csb/modeline-active-p ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and csb/modeline-current-window
         (eq (csb/modeline-get-current-window) csb/modeline-current-window))))

(defun csb/modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (let ((win (csb/modeline-get-current-window)))
    (setq csb/modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun csb/modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq csb/modeline-current-window nil))

(add-hook 'pre-redisplay-functions #'csb/modeline-set-selected-window)



(setq csb/modeline-enable-word-count nil)

;; From https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el#L1117
(defun csb/modeline-selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (csb/modeline-active-p))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat " "
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (csb/modeline-column end)
                                            (csb/modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       (t
                        (format "%dC" (- end beg))))
                 (when csb/modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'italic))))
;; TODO: ^^ Should probably be mode-line-highlight but I want italics and not
;; bold.  Maybe different color; but that also is a theme thing?


(use-package mlscroll
  :straight t
  :config
  (setq mlscroll-shortfun-min-width nil)
  (setq mlscroll-alter-percent-position nil)
  (setq mlscroll-right-align t)
  (setq mlscroll-width-chars 16)
  (setq mlscroll-minimum-current-width 12)
  (mlscroll-mode 1))


;; https://occasionallycogent.com/custom_emacs_modeline/index.html
(setq-default mode-line-format
              '(
                :eval
                (csb/align-mode-line
                 ;; left
                 '("%e" mode-line-front-space
                   " "
                   ;; don't display the '-' for local directories, just '@' on remote
                   (:eval (when (and (stringp default-directory) (file-remote-p default-directory))
                            'mode-line-remote))
                   (:eval (propertize "%b" 'face 'bold))
                   " "
                   "%l:%c "
                   (:eval (csb/modeline-selection-info))
                   )
                 ;; right
                 '(" "
                   mode-line-modes
                   " "
                   (vc-mode vc-mode)
                   " "
                   mode-line-mule-info
                   mode-line-client
                   (:eval (csb/lsp-state-icon))
                   (:eval (csb/mode-line-buffer-file-state-icon))
                   mode-line-frame-identification
                   " "
                   mode-line-misc-info
                   "|")
                 (csb/mlscroll-mode-right-reserved))))


;; Creates a "draw" of sorts for minor modes
;; https://manuel-uberti.github.io/emacs/2018/03/10/moody-and-minions/
(use-package minions
  :straight t
  :init
  (defface csb/minions-mode-line-lighter '((t (:height 1.1))) "")
  :config
  (setq minions-mode-line-delimiters nil)
  (setq minions-mode-line-lighter "🔨")
  (setq minions-mode-line-face 'csb/minions-mode-line-lighter)
  (minions-mode 1))


;; Alternative tabs
(use-package centaur-tabs
  :straight t
  :demand
  :after (projectile counsel)
  :init
  ;; https://github.com/ema2159/centaur-tabs/issues/29
  ;; adapated from centaur-tabs-adjust-buffer-order
  (defun csb/centaur-tabs-adjust-buffer-order-lexicographically ()
    ""
    ;; Don't trigger by centaur-tabs command, it's annoying.
    (unless (or (string-prefix-p "centaur-tabs" (format "%s" this-command))
                (string-prefix-p "mouse-drag-header-line" (format "%s" this-command))
                (string-prefix-p "(lambda (event) (interactive e)" (format "%s" this-command)))
      ;; Just continue when the buffer has changed.
      (when (and centaur-tabs-adjust-buffer-order
                 (not (eq (current-buffer) centaur-tabs-last-focused-buffer)) ;;???
                 (not (minibufferp)))
        (let* ((current (current-buffer))
               (current-group (cl-first (funcall centaur-tabs-buffer-groups-function))))
          (setq centaur-tabs-last-focused-buffer current)
          ;; Just continue if two buffers are in the same group.
          (when (string= current-group centaur-tabs-last-focused-buffer-group)
            (let* ((bufset (centaur-tabs-get-tabset current-group))
                   (current-group-tabs (centaur-tabs-tabs bufset)))
              (setq new-group-tabs (sort current-group-tabs
                                         (lambda (x y)
                                           (string< (buffer-name (car x)) (buffer-name (car y))))))
              (set bufset new-group-tabs)
              (centaur-tabs-set-template bufset nil)
              (centaur-tabs-display-update)))
          (setq centaur-tabs-last-focused-buffer-group current-group)))))
  (defun csb/centaur-tabs-group-by-mode ()
    (interactive)
    (setq centaur-tabs-buffer-groups-function 'csb/centaur-tabs-mode-buffer-groups)
    (centaur-tabs-force-update))
  (defun csb/centaur-tabs-mode-buffer-groups ()
     (cond
      ((or (string-prefix-p "*" (buffer-name))
           (string-prefix-p " *" (buffer-name)))
       '("Emacs"))
      (nnnt  (list (symbol-name major-mode)))))
  :bind
  ;; TODO: Switch to only using terminal style PageUp/Down for fewer conflicts?
  ([(C-S-iso-lefttab)] . centaur-tabs-backward)
  ([(control tab)] . centaur-tabs-forward)
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t m" . csb/centaur-tabs-group-by-mode)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-adjust-buffer-order 't)
  (setq centaur-tabs-adjust-buffer-order-function
        'csb/centaur-tabs-adjust-buffer-order-lexicographically)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-set-bar 'over) ;; Theme dependent?
  (setq centaur-tabs-style "box")  ;; Theme dependent?
  (defun centaur-tabs-hide-tab (x)
     (let ((name (format "%s" x)))
       (or
        (string-prefix-p "*" name)
        (string-prefix-p " *which-key*" name)
        (string-prefix-p " *Minibuf" name)
        (string-prefix-p "COMMIT_EDITMSG" name)
        (and (string-prefix-p "magit" name)
             (not (file-name-extension name))))))
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))


(use-package dashboard
  :straight t
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "🐧🐧🐧 Weclome to Emacs! 🐃🐃🐃")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))
