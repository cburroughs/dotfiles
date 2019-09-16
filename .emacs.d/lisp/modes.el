;; emacs modes I use, their customization, and setup.

(transient-mark-mode t) ; redundant, still does not work in ubuntu
(delete-selection-mode t) ; delete selected text

;; more paren highlighting goodness
(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))

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


;; TODO: replace with org capture. "Capture lets you quickly store notes with
;; little interruption of your work flow. Org's method for capturing new items
;; is heavily inspired by John Wiegley excellent remember.el package. "
;; ;; remember
;; (require 'remember)
;; (setq remember-data-file "~/Documents/org/notes.txt") ;; will change
;; (global-set-key (kbd "C-c r") 'remember)
;; (defun wicked/remember-review-file ()
;;   "Open `remember-data-file'."
;;   (interactive)
;;   (find-file-other-window remember-data-file))
;; (global-set-key (kbd "C-c R") 'wicked/remember-review-file)


;; -------------
;; org mode stuff


;; TODO: https://emacs.stackexchange.com/questions/7432/make-visual-line-mode-more-compatible-with-org-mode/12437
;; ^^ How can I get visual-line-mode/visual-fill-column mode to play nice with org-mode. By not wrapping headings?


(use-package find-lisp)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :requires (find-lisp)
  :init
  ;; TODO: Not sure this is still needd
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; Undefine C-c [ and C-c ] since this breaks my
               ;; org-agenda files when directories are include It
               ;; expands the files in the directories individually
               (org-defkey org-mode-map "\C-c[" 'undefined)
               (org-defkey org-mode-map "\C-c]" 'undefined))
            'append)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))

  :config
  (setq org-log-done t)
  (setq org-todo-keywords '("TODO(t)" "WAITING(w)" "|"
                            "DONE(d)" "CANCELED(c)"))
  (setq org-agenda-include-diary t)
  (setq org-agenda-include-all-todo t)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-files (find-lisp-find-files  "~/Documents/org" "\.org$")))

(use-package org-bullets
  :ensure t
  :pin melpa-stable
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "✸" "◆" "⚫" "✿" "◇" "○")))


;; "sidebar" outline for org-mode and anything that supports imenu
(use-package imenu-list
  :ensure t
  :pin melpa-stable
  :bind ([f9] . imenu-list-smart-toggle))


;; sql indenting
(use-package sql-indent
  :ensure t
  :pin gnu
  :hook (sql-mode . sqlind-minor-mode))


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

;; TODO: easy way to change this the unreadable style common in java code
;; Probably don't want this on by default but useful if I start
;; coding in maximized windows
(use-package highlight-80+
  :defer 2)

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


;; whitespace
(use-package show-wspace
  :defer 2)

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
  :defer 1
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
