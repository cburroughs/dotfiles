;; emacs modes I use, their customization, and setup.

(transient-mark-mode t) ; redundant, still does not work in ubuntu
(delete-selection-mode t) ; delete selected text

;; more paren highlighting goodness
(use-package highlight-parentheses
             :ensure t
             :pin melpa-stable
             :config
             (highlight-parentheses-mode)
             (add-hook 'after-change-major-mode-hook
                       (lambda () (highlight-parentheses-mode 1))))



;; from esk : display column number in modeline
(setq column-number-mode t)
;; from esk: says what it does
(set-default 'indicate-empty-lines t)

(setq visible-bell t)

(setq diff-switches "-u")
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;; Finally found this.  page/updown can be undone
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
;; TODO: deprecated in 24,
;; https://www.emacswiki.org/emacs/IcompleteMode
;; http://superuser.com/questions/811454/why-was-iswitchb-removed-from-gnu-emacs-24
(require 'iswitchb)
(iswitchb-mode 1)


;; remember
(require 'remember)
;;(require 'remember-autoloads)
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
(setq org-hide-leading-stars t)
; Should this stuff all done only after loading org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords '("TODO" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-deadline-warning-days 14)
(require 'find-lisp)
(setq org-agenda-files (find-lisp-find-files  "~/Documents/org" "\.org$"))
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined))
          'append)

;; "sidebar" outline for org-mode and other outlines
(use-package outline-toc
  :ensure t
  :pin melpa
  :bind ([f9] . outline-toc-mode))


;; sql indenting
(use-package sql-indent
             :ensure t
             :pin gnu
             :init
             (eval-after-load "sql"
               '(load-library "sql-indent"))
             :config
             (setq sql-indent-offset 4))


;; dot files
(use-package graphviz-dot-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.dot$" . graphviz-dot-mode))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :config
  ;; Remove once this is in a newer markdown-mode release
  ;; https://github.com/jrblevin/markdown-mode/pull/252/files
  (set-face-attribute 'markdown-inline-code-face nil
                      :inherit '(markdown-code-face font-lock-constant-face))
  :mode (("\\.mdown" . markdown-mode)
         ("\\.markdown" . markdown-mode)
         ("\\.md" . markdown-mode)
         ("\\.page" . markdown-mode)))

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
(use-package kill-ring-search
             :ensure t
             :pin melpa-stable
             :bind ("M-C-y" . kill-ring-search))


;; far-search, finally something that searches across buffers!
;; http://github.com/aemoncannon/far-search-mode/tree/master
(require 'far-search)


;; whitespace
(require 'show-wspace)
(defun show-white-space ()
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

(use-package rainbow-mode
             :ensure t
             :pin gnu
             :defer t)


;; nice commit messages
(require 'git-commit)

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-enable-undo-in-region nil)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; see https://github.com/syl20bnr/spacemacs/issues/12110
;; Setting to about 10x the default
(setq undo-limit (* 1024 80 10))
(setq undo-strong-limit (* 2 undo-limit))
(setq undo-outer-limit 24000000)
