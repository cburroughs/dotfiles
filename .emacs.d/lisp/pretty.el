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


;;"LIN locally remaps the hl-line face to a style that is optimal for major
;; modes where line selection is the primary mode of interaction."
(use-package lin
  :straight (lin :type git :host gitlab :repo "protesilaos/lin")
  :config
  (add-to-list 'lin-foreign-hooks 'completion-list-mode-hook)
  (lin-add-to-many-modes))
;; TODO: ^^ Customize faces


(defun csb/real-ish-buffer-p (&optional buffer)
   "Return t if the current buffer is a real buffer.  A buffer is
   real if it belongs to a file, but also if it is something like
   *scratch* that you interact with, or a buffer that just
   doesn't have a file yet...  It is sort of fuzzy, call it 'real-ish"
   (let* ((buffer (or buffer (buffer-base-buffer)))
          (buffer-name (buffer-name buffer)))
     (or (buffer-file-name buffer)
         (and (not (string-prefix-p "*" buffer-name))
              (not (string-prefix-p " " buffer-name)))
         (or (string-equal buffer-name "*scratch*")
             (string-equal buffer-name "*dashboard*")
             (string-equal buffer-name "*Ibuffer*")))))

;; "solaire-mode is an aesthetic plugin designed to visually distinguish
;; "real" buffers (i.e. file-visiting code buffers where you do most of your
;; work) from "unreal" buffers (like popups, sidebars, log buffers, terminals,
;; etc) by giving the latter a slightly different -- often darker --
;; background:"
(use-package solaire-mode
  :straight t
  :config
  (setq solaire-mode-real-buffer-fn #'csb/real-ish-buffer-p)
  (solaire-global-mode +1))


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
  ;; Override themes with hammer. NOTE: :overline and :underline are still finicky
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  ;; Setto something neutral for now
  ;; https://github.com/jdtsmith/mlscroll/issues/12
  (setq mlscroll-in-color "gray40")
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
                   (vc-mode vc-mode)
                   " "
                   mode-line-mule-info
                   mode-line-client
                   (:eval (csb/lsp-state-icon))
                   (:eval (csb/mode-line-buffer-file-state-icon))
                   mode-line-frame-identification
                   mode-line-misc-info)
                 (csb/mlscroll-mode-right-reserved))))

;; Keep the modline unicode symbols single color
;; Otherwise wit emacs-28 and harfbuzz we get colorfull madness
;; https://archive.casouri.cat/note/2021/fontset/index.html
(create-fontset-from-fontset-spec
 (font-xlfd-name
  (font-spec :name "DejaVu Sans Mono"
             :size 11
             :registry "fontset-csb modeline")))

(set-fontset-font
 "fontset-csb modeline"
 'unicode (font-spec :name "Symbola"))

(set-face-attribute 'mode-line nil :fontset "fontset-csb modeline")


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
