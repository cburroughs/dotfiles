;; Chris Burroughs
;; emacs modes I use, their customization, and setup.

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
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

; Finally found this.  page/updown can be undone
(setq scroll-preserve-screen-position t)

 
(defun esk-pp-json ()
  "Pretty-print the json object following point."
  (interactive)
  (require 'json)
  (let ((json-object (save-excursion (json-read))))
    (switch-to-buffer "*json*")
    (delete-region (point-min) (point-max))
    (insert (pp json-object))
    (goto-char (point-min))))


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


;; Better twiki editing
(require 'erin)
(add-to-list 'auto-mode-alist '(".*firefox.*twiki.*" . erin-mode))


;; sql indenting
;; http://www.emacswiki.org/cgi-bin/wiki/download/sql-indent.el
(eval-after-load "sql"
  '(load-library "sql-indent"))
(eval-after-load "sql-indent"
  '(progn
     (setq sql-indent-offset 4)))



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



;; far-search, finally something that searches across buffers!
;; http://github.com/aemoncannon/far-search-mode/tree/master
(require 'far-search)

(require 'nav)


;; whitespace
(require 'show-wspace)
(defun show-white-spacee ()
    (iteractive)
    (show-ws-toggle-show-hard-spaces)
    (show-ws-toggle-show-tabs)
    (show-ws-toggle-show-trailing-whitespace))


;; http://xahlee.org/emacs/command-frequency.html
;; http://nflath.com/2009/08/command-frequency-mode/
;; Keep track of commands used, learning is good!
;; todo: how to share this file among instances
;(setq-default command-frequency-table-file "~/.emacs_frequencies")
;(require 'command-frequency)
;(command-frequency-table-load)
;(command-frequency-mode 1)
;(command-frequency-autosave-mode 1)


;; yasnippet goodness!!

;; additional snippet packs, the least bad thing seems to be dump them
;; into one giant install snippet folder incase format or things
;; change

;; http://github.com/rejeep/yasnippets
;; http://github.com/madsdk/yasnippets-latex

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/emacs/site-lisp/yasnippet-0.6.1c/snippets")