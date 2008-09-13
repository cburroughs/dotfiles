; Chris Burroughs
; Modes I use and their custimization.

; allows shift and arrows for selection
(require 'pc-select)

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



; remember
(require 'remember)
;(require 'remember-autoloads)
(setq remember-data-file "~/emacs/org/notes.txt") ;; will change
(global-set-key (kbd "C-c r") 'remember)
(defun wicked/remember-review-file ()
  "Open `remember-data-file'."
  (interactive)
  (find-file-other-window remember-data-file))
(global-set-key (kbd "C-c R") 'wicked/remember-review-file)

;; -------------
; org mode stuff
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords '("TODO" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-deadline-warning-days 14)
;; TODO make this more general between computers
(setq org-agenda-files (list "~/Documents/org/home.org"))

;; Give agenda normal emacs keybindings
(eval-after-load "org"
  '(progn
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)
     (define-key global-map "\C-cl" 'org-store-link)
     (define-key global-map "\C-ca" 'org-agenda)
     (define-key global-map [(control shift tab)] 'tabbar-backward)
     (define-key global-map[(control tab)]       'tabbar-forward)))


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

