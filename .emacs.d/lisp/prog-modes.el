;; Programming modes in a separate file


;;; Javascript
(use-package js2-mode
             :ensure t
             :pin gnu)


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


;; The crazy wrod of nxhtml mode
(defun load-nxhtml-mode ()
  "Load nxhtml mode from it's latest directory of crazyness"
  (interactive)
  (load "~/local_install/nxhtml/nxhtml-1.9.32-090804/autostart.el"))


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
            '(lambda () (c-set-offset 'substatement-open 0)))
  ;; one of these does the trick
  (add-hook 'malabar-mode-hook 
            '(lambda () (c-set-offset 'substatement-open 0)))
  (eval-after-load "malabar-mode"
    '(lambda () (c-set-offset 'substatement-open 0))))


;;; accept evil tabs
(defun faust-make-deal ()
  (interactive)
  (setq-default indent-tabs-mode 1)
  (require 'smarttabs))


(defun faust-change-mind ()
  (interactive)
  (setq-default indent-tabs-mode nil))
  

;; java mode+++
;; (when (not (my-gentoo?))
;;   (progn
;;     (require 'cedet)
;;     (semantic-load-enable-minimum-features) ;; or enable more if you wish
;;     (require 'malabar-mode)
;;     (setq malabar-groovy-lib-dir 
;;           "~/local_install/malabar/malabar-1.4-SNAPSHOT/lib/")
;;     (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))))


(defun java-4-space ()
  (interactive)
  (setq c-basic-offset 4))

;; who does this.
(defun java-2-space ()
  (interactive)
  (setq c-basic-offset 2))


(require 'vcl-mode)
(setq vcl-indent-level 4)


(setq ruby-deep-indent-paren nil)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))

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
