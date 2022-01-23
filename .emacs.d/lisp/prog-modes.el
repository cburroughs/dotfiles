;; -*- lexical-binding: t -*-

;; Programming modes in a separate file

(require 'use-package)

;; imenu

;; Turn on imenu in any mode that supports it
;; https://www.emacswiki.org/emacs/ImenuMode
(defun csb/try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'csb/try-to-add-imenu)


;; Auto-recan induces human noticible lag
;; (setq imenu-auto-rescan 't)
;; (setq imenu-auto-rescan-maxout (* 1024 1024))

;; TODO: Need some sort of satety latch for large files
(defun csb/maybe-imenu-rescan ()
  ;; https://www.emacswiki.org/emacs/ImenuMode#toc4
  (imenu--menubar-select imenu--rescan-item))
(run-with-idle-timer 5 t 'csb/maybe-imenu-rescan)
(add-hook 'focus-out-hook 'csb/maybe-imenu-rescan)



;; "dumb" jump to definition

;; TODO: Consider M-g basd keybinding with hydra
(use-package dumb-jump
  :straight t
  :config
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-aggressive nil)
  (dumb-jump-mode))


;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq tab-always-indent 't)

;; json
(use-package json-mode
             :straight t
             :mode "\\.json$")

;;; Python

(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))


;;;; Lisps


;; Useful stuff for almost all c modes
;; http://nflath.com/2009/08/cc-and-java-customizations/
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-subword-mode 1)))


;;; Java
(defun mrallen-java-mode ()
  (interactive)
  (add-hook 'java-mode-hook
            (lambda () (c-set-offset 'substatement-open 0))))

;;; accept evil tabs
(defun faust-make-deal ()
  (interactive)
  (setq-default indent-tabs-mode 1)
  (require 'smarttabs))


(defun faust-change-mind ()
  (interactive)
  (setq-default indent-tabs-mode nil))


(defun java-4-space ()
  (interactive)
  (setq c-basic-offset 4))

;; who does this.
(defun java-2-space ()
  (interactive)
  (setq c-basic-offset 2))

(defun nxml-4-space ()
  (interactive)
  (setq nxml-child-indent 4 nxml-attribute-indent 4))

(defun nxml-2-space ()
  (interactive)
  (setq nxml-child-indent 4 nxml-attribute-indent 2))

(use-package vcl-mode
             :straight t
             :mode "\\.vcl$")

(setq ruby-deep-indent-paren nil)

(use-package web-mode
             :straight t
             :mode ("\\.jinja$" "\\.mustache$"))

(defun phab-php-mode ()
  (interactive)
  (setq c-default-style "k&r"
        c-basic-offset 2)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'statement-cont '+)
  (c-set-offset 'topmost-intro-cont '+)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-cont-nonempty '+))


;; grooy
(use-package groovy-mode
  :straight t
  :mode "\\.groovy$")


;; Language Server Time


;; TODO: I would like to be able to stick with melpa-stable
;; TODO: in future versions, check out lsp-deferred
(use-package lsp-mode
  :straight t
  :hook ((rust-mode . lsp)
         (python-mode . lsp)
         (python-mode . (lambda () (flymake-mode 0))))
  :commands lsp
  :config
  ;; Remove this crazy hack after https://github.com/emacs-lsp/lsp-ui/issues/234
  ;; is stable
  (with-eval-after-load 'lsp
    (run-at-time "1 sec" 4 'flymake-mode 0))
  (setq lsp-auto-guess-root 't)
  (setq lsp-enable-snippet nil)
  (setq lsp-highlight-sumbol-at-point nil)
  (setq lsp-enable-symbol-highlighting nil)

  (defun csb/lsp-ui-doc-toggle ()
    (interactive)
    (if (get 'csb/lsp-ui-doc-toggle 'state)
        (progn
          (put 'csb/lsp-ui-doc-toggle 'state nil)
          (lsp-ui-doc-hide))
      (progn
        (put 'csb/lsp-ui-doc-toggle 'state 't)
        (lsp-ui-doc-show))))
  :config
  (setq lsp-completion-provider :capf))

  

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :bind (("C-c q" . csb/lsp-ui-doc-toggle))
  :config
  ;; not a fan of so much noise
  (setq lsp-ui-flycheck-enable nil)
  (setq lsp-prefer-flymake :none)
  ;; the foo/bar format is less useful than the built in regex
  ;; revisit after https://github.com/emacs-lsp/lsp-mode/issues/784 (post 6.0)
  (setq lsp-ui-imenu-enable nil)
  ;; Remove this crazy hack after https://github.com/emacs-lsp/lsp-ui/issues/234
  ;; is stable
  (with-eval-after-load 'lsp-ui
    (run-at-time "1 sec" 4 'flymake-mode 0))

  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-peek-enable nil)
  ;; I like the inline docs, but not how it pops up all the freaking time, see
  ;; toggle fn above
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header 't)
  (setq lsp-ui-doc-include-signature 't)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-use-childframe 't))


(provide 'csb/prog-modes)
