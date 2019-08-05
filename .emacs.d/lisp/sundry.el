;; misc changes and variable settings

(use-package esup
  :ensure t
  :defer 1)


;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; copy-paste should work with other X clients
;; duh; only do this when you have x
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; change spelling
(setq-default ispell-program-name "aspell")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; graphviz dot mode requires this to be set, unclear how it is different from the above
(setq-default default-tab-width 4)

;; prefer hippie-expand over deavrez
(global-set-key (kbd "M-/") 'hippie-expand)

;; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Try to avoid giving buffers dumb names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; find files recursivly
(require 'find-recursive)

;; default to firefox in new tab
(setq browse-url-default-browser 'browse-url-firefox-program)
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab 't) ; why does this not work?


;; Better buffer switching
;; TODO: deprecated in 24,
;; https://www.emacswiki.org/emacs/IcompleteMode
;; http://superuser.com/questions/811454/why-was-iswitchb-removed-from-gnu-emacs-24
(require 'iswitchb)
(iswitchb-mode 1)


(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (setq projectile-known-projects-file "~/.config/emacs/projectile-bookmarks.el")
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "build/")
  (add-to-list 'projectile-globally-ignored-directories "dist/")
  (add-to-list 'projectile-globally-ignored-directories "target/")
  (projectile-register-project-type 'pants '("pants.ini"))
  (projectile-mode +1))


;; TODO: Can projectile fully replace this old vender fork?
(require 'anything)
(require 'anything-find-project-resources)
;;(global-set-key (kbd "M-r") 'anything-find-resource)
(add-to-list `anything-find-resource--project-root-files '"build.sbt")
(add-to-list `anything-find-resource--project-root-files '"pants.ini")
(add-to-list `anything-find-resource--ignore-dirs '"dist/")
(add-to-list `anything-find-resource--ignore-dirs '"target/")
(add-to-list `anything-find-resource--ignore-files '"*.pyc")

(autoload 'columnize-text "columnize"
  "Formats a list of items into columns (pillars)" t)

;; http://www.emacswiki.org/emacs/FillParagraph
;; http://jblevins.org/projects/markdown-mode/
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.\\|[ \t]*: ")


(use-package lorem-ipsum
             :ensure t)

(use-package neotree
  :ensure t
  :pin melpa-stable
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-autorefresh nil))


;; http://pragmaticemacs.com/emacs/world-clock-in-emacs/
;; for display-time-world
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Los_Angeles")
        ("America/New_York" "New York")
        ("America/Vancouver" "Vancouver")
        ("Europe/Berlin" "Berlin")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Asia/Seoul" "Seoul")
        ("Asia/Shanghai" "Bejing/Shanghai")
        ("Asia/Tokyo" "Tokyo")))


;; NOTE: Neither ivy nor counsel are "enabled" to take over standard buffer or
;; file operations.  But a number of other handy functions are enabled.
(use-package ivy
  :ensure t
  :pin melpa-stable
  :demand t
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 0))


(use-package swiper
  :ensure t
  :after ivy
  :pin melpa-stable
  ;; https://oremacs.com/2019/04/07/swiper-isearch/ Maybe revsit fully switching
  ;; to swiper instead of isearch in the future?  Sadly seems prone to 100% cpu
  ;; hangs.  https://github.com/abo-abo/swiper/issues/925 is perhaps a relevant
  ;; issue
  :bind (("M-s" . swiper)
         ("M-r" . swiper-backward)
         ;;("M-s" . isearch-forward)
         ;;("M-r" . isearch-backward)
         ;; https://oremacs.com/2016/07/29/brand-new-swiper-all/
         ("C-c s" . swiper-all))
  :config
  (setq swiper-stay-on-quit nil))


(use-package counsel
  :ensure t
  :after ivy
  :pin melpa-stable
  :demand t
  ;; remap only some standard keybindings; add new ones
  :bind (("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f1> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep))
  :config
  ;; Why does this have to be defined here and not above?
  (global-set-key (kbd "C-c C-j") 'counsel-imenu)
  ;; Provisional, unclear on side effects
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))



;;; Windows and jumping around

;; "undo" for window changes
(when (fboundp 'winner-mode)
  (winner-mode 1))

(use-package ace-window
  :ensure t
  ;; ran into a bug with the stable one?
  :pin melpa
  :bind (("M-o" . ace-window)))


;; Do I actually end up using this?
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-2)
         ("C-'" . avy-goto-line))
  :config (setq avy-all-windows 't))


;; Do I actually end up using this?
(use-package eyebrowse
  :ensure t
  :pin melpa-stable
  :config
  (eyebrowse-mode t))
