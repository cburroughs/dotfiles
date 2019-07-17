;; Programming modes in a separate file


;; Completion


;; https://www.emacswiki.org/emacs/CompanyMode
(defun company-except-in-minibuffer ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (company-indent-or-complete-common)))

;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq tab-always-indent 'complete)

;; The above together are needed to make company mode not go totally crazy
;; fighting with existing auto-compete or indentation.  Python indentation is
;; still a little different, but hitting S-TAB works about as well as repeated
;; TAB to cycle
(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :bind (("<tab>" . company-except-in-minibuffer))
  :config
  (setq company-tooltip-limit 16)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay nil)
  (setq company-selection-wrap-around t)
  ;; leave it to hippie
  (setq company-backends (delete 'company-dabbrev company-backends))
  (company-tng-configure-default))


;;; Javascript
(use-package js2-mode
             :ensure t
             :pin gnu
             :defer t)


(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda () 
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; json
(use-package json-mode
             :ensure t
             :pin melpa-stable
             :mode "\\.json$")

;;; Python

(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))


;;;; Lisps


;; Useful stuff for almost all c modes
;; http://nflath.com/2009/08/cc-and-java-customizations/
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-subword-mode 1)))


;;; Java
(defun mrallen-java-mode ()
  (interactive)
  (add-hook 'java-mode-hook
            '(lambda () (c-set-offset 'substatement-open 0))))

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
             :ensure t
             :pin gnu
             :mode "\\.vcl$")

(setq ruby-deep-indent-paren nil)

(use-package web-mode
             :ensure t
             :pin melpa-stable
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
