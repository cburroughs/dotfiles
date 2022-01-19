;; -*- lexical-binding: t -*-

;; fancy completion and (many) friends


;; Hippie 💓💓💓 so simple; so often better than fancy alternatives

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


;;  completing-read


;; ;; Better buffer switching
;; ;; TODO: deprecated in 24,
;; ;; https://www.emacswiki.org/emacs/IcompleteMode
;; ;; http://superuser.com/questions/811454/why-was-iswitchb-removed-from-gnu-emacs-24
;; (use-package iswitchb
;;   :config
;;   (iswitchb-mode 1))


;; ;; NOTE: Neither ivy nor counsel are "enabled" to take over standard buffer or
;; ;; file operations.  But a number of other handy functions are enabled.
;; (use-package ivy
;;   :straight t
;;   :demand t
;;   :bind ("C-c C-r" . ivy-resume)
;;   :config
;;   (setq ivy-use-virtual-buffers nil)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (ivy-mode 0))


;; (use-package swiper
;;   :straight t
;;   :after ivy
;;   ;; https://oremacs.com/2019/04/07/swiper-isearch/ Maybe revsit fully switching
;;   ;; to swiper instead of isearch in the future?  Sadly seems prone to 100% cpu
;;   ;; hangs.  https://github.com/abo-abo/swiper/issues/925 is perhaps a relevant
;;   ;; issue
;;   :bind (("M-s" . swiper)
;;          ("M-r" . swiper-backward)
;;          ;;("M-s" . isearch-forward)
;;          ;;("M-r" . isearch-backward)
;;          ;; https://oremacs.com/2016/07/29/brand-new-swiper-all/
;;          ("C-c s" . swiper-all))
;;   :config
;;   (setq swiper-stay-on-quit nil))


;; (use-package counsel
;;   :straight t
;;   :after ivy
;;   :demand t
;;   ;; remap only some standard keybindings; add new ones
;;   :bind (("<f1> f" . counsel-describe-function)
;;          ("<f1> v" . counsel-describe-variable)
;;          ("<f1> l" . counsel-find-library)
;;          ("<f1> u" . counsel-unicode-char)
;;          ("C-c g" . counsel-git)
;;          ("C-c j" . counsel-git-grep))
;;   :config
;;   ;; Why does this have to be defined here and not above?
;;   (global-set-key (kbd "C-c C-j") 'counsel-imenu)
;;   ;; Provisional, unclear on side effects
;;   (setq enable-recursive-minibuffers t)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Minibuffer and Completions in Tandem
(use-package mct
  :straight t
  :init
  (setq mct-hide-completion-mode-line t)
  (setq mct-completion-passlist '(switch-to-buffer))
  (mct-minibuffer-mode 1))

;; completion-in-region

;; https://www.emacswiki.org/emacs/CompanyMode
;; (defun company-except-in-minibuffer ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (derived-mode-p 'prog-mode)
;;         (company-indent-or-complete-common)
;;       (indent-for-tab-command))))


;; ;; The above together are needed to make company mode not go totally crazy
;; ;; fighting with existing auto-compete or indentation.  Python indentation is
;; ;; still a little different, but hitting S-TAB works about as well as repeated
;; ;; TAB to cycle
;; (use-package company
;;   :straight t
;;   :defer t
;;   :hook (prog-mode . company-mode)
;;   :bind (("M-`" . company-complete)
;;          (:map company-active-map
;;                ("C-n" . company-select-next)
;;                ("C-p" . company-select-previous)))
;;   :config
;;   (setq company-tooltip-limit 16)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-idle-delay nil)
;;   (setq company-selection-wrap-around t)
;;   ;; leave it to hippie
;;   (setq company-backends (delete 'company-dabbrev company-backends)))
