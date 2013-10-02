;; Programming modes in a separate file


;;; Javascript

;; new js2
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda () 
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; json
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))


;;; Python

;; Python mode for ubuntu.  (has working indentation)
;; TODO: this really should not be needed
(when (not (my-gentoo?))
           (autoload 'python-mode "python-mode" "Python Mode." t)
           (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
           (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

(if (getenv "PYTHONPATH") ; if nil
    (when (not (string-match "site-lisp" (getenv "PYTHONPATH")))
      (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" 
                                   (expand-file-name "~/emacs/site-lisp"))))
  (setenv "PYTHONPATH"  (expand-file-name "~/emacs/site-lisp")))

(eval-after-load "python-mode"
  '(require 'pycomplete))
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
;(setq interpreter-mode-alist(cons '("python" . python-mode)
;                             interpreter-mode-alist))


;;;; Lisps

;;; Arc
;; arc stuff from http://github.com/nex3/dotfiles/
(autoload 'arc-mode "arc" nil t)
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
; TODO: inferior arc

;;; Clojure
;; ref: http://riddell.us/tutorial/slime_swank/slime_swank.html
;; and some READMEs...
(require 'clojure-mode)
;(add-to-list 'load-path "~/prog/github/swank-clojure/src/emacs")
(require 'swank-clojure)

;(require 'swank-clojure-autoload)
;(swank-clojure-config
; (setq swank-clojure-jar-path "~/local_install/clojure/clojure.jar")
; (setq swank-clojure-extra-classpaths
;       (mapcar 'trim-string
;               (cons "~/local_install/clojure/clojure-contrib.jar"
;                     (split-string
;                      (shell-command-to-string
;                       "gen_classpath -r -L ~/local_install/clojure/lib"))))))





;;; Common Lisp

;; slime
;(require 'slime)
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; todo: how to do this for multiple slime lisps?
;;(setq inferior-lisp-program "sbcl")

;;; Scheme
;; see external.el for scheme program name 


;; C#, YA RLY
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


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
(when (not (my-gentoo?))
  (progn
    (require 'cedet)
    (semantic-load-enable-minimum-features) ;; or enable more if you wish
    (require 'malabar-mode)
    (setq malabar-groovy-lib-dir 
          "~/local_install/malabar/malabar-1.4-SNAPSHOT/lib/")
    (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))))


;; I suppose string templates are closest to programming
(require 'stringtemplate-mode)
(add-to-list 'auto-mode-alist '("\\.st$" . stringtemplate-mode))


(defun java-4-space ()
  (interactive)
  (setq c-basic-offset 4))

;; who does this.
(defun java-2-space ()
  (interactive)
  (setq c-basic-offset 2))


;;; scala

(require 'scala-mode-auto)
(eval-after-load "scala-mode"
  '(lambda () (define-key scala-mode-map [(tab)] 'scala-indent-line)))

(require 'vcl-mode)


(setq ruby-deep-indent-paren nil)