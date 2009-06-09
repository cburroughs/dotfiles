;; Chris Burroughs
;; Modes I use and their custimization.

;; allows shift and arrows for selection
(require 'pc-select)
(transient-mark-mode t) ; redundant, still does not work in ubuntu
(delete-selection-mode t) ; delete selected text

;; more paren highlighting goodness
;; todo: add more colors
(require 'highlight-parentheses)
(highlight-parentheses-mode)
(add-hook 'after-change-major-mode-hook
          (lambda () (highlight-parentheses-mode 1)))


;; from esk : display column number in modeline
(setq column-number-mode t)
;; from esk: says what it does
(set-default 'indicate-empty-lines t)

(setq visible-bell t)

(setq diff-switches "-u")

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


; Finally found this.  page/updown can be undone
(setq scroll-preserve-screen-position t)

;; looking into 
;; (add-hook 'speedbar-load-hook
;;    '(lambda ()
;;    (setq speedbar-update-speed 5
;;      speedbar-track-mouse-flag t
;;      speedbar-activity-change-focus t)))

;; slime
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program "sbcl")

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
     (add-hook 'js2-mode-hook 'coding-hook)
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

 
(defun esk-pp-json ()
  "Pretty-print the json object following point."
  (interactive)
  (require 'json)
  (let ((json-object (save-excursion (json-read))))
    (switch-to-buffer "*json*")
    (delete-region (point-min) (point-max))
    (insert (pp json-object))
    (goto-char (point-min))))


(require 'js-comint)
(setq inferior-js-program-command "rhino")
(add-hook 'js2-mode-hook '(lambda () 
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; Better buffer switching
(require 'iswitchb)
(iswitchb-mode 1)

;; tabbar is pretty awesome
(require 'tabbar)
(tabbar-mode t)
(global-set-key [(C-S-iso-lefttab)] 'tabbar-backward) ; why funy binding?
(global-set-key [(control tab)]       'tabbar-forward)
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

; Don't cycle *scratch* and friends
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (find (aref (buffer-name buffer) 0) " *"))
    	   (buffer-list))))




; remember
(require 'remember)
;(require 'remember-autoloads)
(setq remember-data-file "~/Documents/org/notes.txt") ;; will change
(global-set-key (kbd "C-c r") 'remember)
(defun wicked/remember-review-file ()
  "Open `remember-data-file'."
  (interactive)
  (find-file-other-window remember-data-file))
(global-set-key (kbd "C-c R") 'wicked/remember-review-file)

;; -------------
; org mode stuff
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; Should this stuff all done only after loading org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords '("TODO" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-deadline-warning-days 14)
;; TODO make this more general between computers
(if (my-gentoo?)
    (setq org-agenda-files (list "~/Documents/org/home.org"))
  (setq org-agenda-files (list "~/Documents/org/cs.org")))

;; Give agenda normal emacs keybindings
(eval-after-load "org"
  '(progn
     (when window-system ; would prefer to do based on if color loaded
       (progn ; implicit?
         ; For reasons I do not understand this does *not* work if you
         ; open the org-file with emacs on the cmd line
         (load "color-theme-colorful-obsolescence")
         (color-theme-colorful-obsolescence)))
     ; These give errors and seem to be unnecessary
     ;(define-key org-agenda-mode-map "\C-n" 'next-line)
     ;(define-key org-agenda-keymap "\C-n" 'next-line)
     ;(define-key org-agenda-mode-map "\C-p" 'previous-line)
     ;(define-key org-agenda-keymap "\C-p" 'previous-line)
     ; Arn't these redundant given I alreay set them?
     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-ca" 'org-agenda)
     (define-key global-map [(control shift tab)] 'tabbar-backward)
     (define-key global-map [(control tab)]       'tabbar-forward)))


;; Better twiki editing for work
(require 'erin)

;; sql indenting
;; http://www.emacswiki.org/cgi-bin/wiki/download/sql-indent.el
(eval-after-load "sql"
  '(load-library "sql-indent"))
(eval-after-load "sql-indent"
  '(progn
     (setq sql-indent-offset 4)))


;; FIXME: have not actually gotten this to work
;; annotations indent
(require 'java-mode-indent-annotations)
(defun my-java-mode-hook ()
  (java-mode-indent-annotations-setup))
(add-hook 'java-mode-hook 'my-java-mode-hook)


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


; dot files
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.mdown" . markdown-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))


;; Save a list of recent files visited.
(recentf-mode 1)

;; I always end up switching to the better mode anyway
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; blah, ubuntu land lacks packages :-(
(when (my-gentoo?)
  (add-to-list 'auto-mode-alist '("\\.html$" . nxhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml$" . nxhtml-mode)))

;; colorize diffs.
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "DodgerBlue2")
     (set-face-foreground 'diff-removed "firebrick2")))

;; TODO: easy way to change this the unreadable style common in java code
;; Probably don't want this on by default but useful if I start
;; coding in maximized windows
(require 'highlight-80+)
(defun enable-highlight-80+ ()
  (interactive)
  (highlight-80+-mode)
  (add-hook 'after-change-major-mode-hook
            (lambda () (highlight-80+-mode 1))))

;; These do not work
(defun hl80 ()
  "Set highlight-80+-mode length to 80. "
  (iteractive)
  (setq highlight-80+-columns 80))

(defun hl100 ()
  "Set highlight-80+-mode length to 100. Is the enough for the java beast? "
  (iteractive)
  (setq highlight-80+-columns 100))


;; might as well use emacs crazy powerful kill-ring
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

(global-set-key "\M-\C-y" 'kill-ring-search)


;; not sure what I want to do with this yet.
;; Sometimes it is nice to move about by visible/screen lines.
;;(require 'visible-lines)


;; Yeah for actionscript
(autoload 'actionscript-mode "actionscript-mode")
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-hook 'actionscript-mode-hook '(lambda () 
                                     (setq c-basic-offset 4)))

;; go haxe!
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



;; far-search, finally something that searches across buffers!
;; http://github.com/aemoncannon/far-search-mode/tree/master
(require 'far-search)

(require 'nav)


;; arc stuff from http://github.com/nex3/dotfiles/
(autoload 'arc-mode "arc" nil t)
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
; TODO: inferior arc


;; whitespace
(require 'show-wspace)
(defun show-white-spacee ()
    (iteractive)
    (show-ws-toggle-show-hard-spaces)
    (show-ws-toggle-show-tabs)
    (show-ws-toggle-show-trailing-whitespace))
