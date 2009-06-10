;; Programming modes in a separate file


;;; Javascript

;; new js2
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; tweaks from http://github.com/technomancy/emacs-starter-kit/tree/master
(eval-after-load 'js2-mode
  '(progn
     ;; my customization
     (setq js2-mirror-mode nil)

      ;; Cosmetics
     ;; Why does this not work?
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             "ƒ")
                             nil)))))
     ;; same; why does this not work?
     (font-lock-add-keywords
      'js2-mode
      '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
         1 font-lock-warning-face t)))
 
     (defun js-lambda () (interactive) (insert "function () {\n}")
       (backward-char 5))
     (add-hook 'js2-mode-hook 'coding-hook) ;; FIXME: need from esk
     (define-key js2-mode-map (kbd "C-c l") 'js-lambda)
 
     ;; Fix js2's crazy indentation
     (define-key js2-mode-map (kbd "TAB") (lambda () (interactive)
                                            (indent-for-tab-command)
                                            (back-to-indentation)))
     (setq js2-bounce-indent-flag nil
           js2-indent-on-enter-key t)

     (defun js-continued-var-decl-list-p ()
       "Return non-nil if point is inside a continued variable declaration list."
       (interactive)
       (let ((start (save-excursion (js-re-search-backward "\\<var\\>" nil t))))
         (and start
              (save-excursion (re-search-backward "\n" start t))
              (not (save-excursion
                     (js-re-search-backward
                      ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))
     
     (defun js-proper-indentation (parse-status)
       "Return the proper indentation for the current line."
       (save-excursion
         (back-to-indentation)
         (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
               (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
               (continued-expr-p (js-continued-expression-p)))
           (cond (ctrl-stmt-indent)
                 ((js-continued-var-decl-list-p)
                  (js-re-search-backward "\\<var\\>" nil t)
                  (+ (current-indentation) js2-basic-offset))
                 ((nth 1 parse-status)
                  (goto-char (nth 1 parse-status))
                  (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                      (progn
                        (skip-syntax-backward " ")
                        (when (= (char-before) ?\)) (backward-list))
                        (back-to-indentation)
                        (cond (same-indent-p
                               (current-column))
                              (continued-expr-p
                               (+ (current-column) (* 2 js2-basic-offset)))
                              (t
                               (+ (current-column) js2-basic-offset))))
                    (unless same-indent-p
                      (forward-char)
                      (skip-chars-forward " \t"))
                    (current-column)))
                 (continued-expr-p js2-basic-offset)
                 (t 0)))))))

(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda () 
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))


;;; Actionscript

(autoload 'actionscript-mode "actionscript-mode")
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-hook 'actionscript-mode-hook '(lambda () 
                                     (setq c-basic-offset 4)))

;;; Haxe

(require 'haxe-mode)
(defconst my-haxe-style
  '("java" (c-offsets-alist . ((case-label . +)
                               (arglist-intro . +)
                               (arglist-close . 0)
                               (cpp-macro . 0))))
  "My haXe Programming Style")
(add-hook 'haxe-mode-hook
          (function (lambda () (c-add-style "haxe" my-haxe-style t))))
(autoload 'haxe-mode "haxe-mode")
(add-to-list 'auto-mode-alist '("\\.hx$" . haxe-mode))

;; (add-hook 'haxe-mode-hook
;;           (function
;;            (lambda ()
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode t)
;;              (setq fill-column 80)
;;              (local-set-key [(return)] 'newline-and-indent))))


;;; Java

;; FIXME: have not actually gotten this to work
;; annotations indent
(require 'java-mode-indent-annotations)
(defun my-java-mode-hook ()
  (java-mode-indent-annotations-setup))
(add-hook 'java-mode-hook 'my-java-mode-hook)


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
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;(add-hook 'clojure-mode-hook 
;          (lambda () (setq inferior-lisp-program "~/bin/clj")))
;; todo: slime


;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(load-library "~/local_install/slime-2009-06-10/slime.el")
;(require 'slime)
(slime-setup)

;;; Common Lisp

;; slime
;(require 'slime)
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; todo: how to do this for multiple slime lisps?
;;(setq inferior-lisp-program "sbcl")

;;; Scheme
;; see external.el for scheme program name 
