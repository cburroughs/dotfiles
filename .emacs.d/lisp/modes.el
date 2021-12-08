;; -*- lexical-binding: t -*-

;; emacs modes I use, their customization, and setup.

(transient-mark-mode t) ; redundant, still does not work in ubuntu
(delete-selection-mode t) ; delete selected text

;; more paren highlighting goodness
(use-package rainbow-delimiters
  :straight t
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


;; -------------
;; org mode stuff


;; TODO: https://emacs.stackexchange.com/questions/7432/make-visual-line-mode-more-compatible-with-org-mode/12437
;; ^^ How can I get visual-line-mode/visual-fill-column mode to play nice with
;; org-mode. By not wrapping headings?


(use-package find-lisp)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :requires (find-lisp ivy counsel)
  :init
  ;; TODO: Can we use this trick in reverse (setting back to default) to keep
  ;; the handful of things (minibuf, files buffers) "normal", and everything
  ;; else ivy?
  (defun csb/refile-with-ivy ()
    (interactive)
    (let ((completing-read-function 'ivy-completing-read))
      (call-interactively 'org-refile)))
  (setq org-directory "~/Documents/org")
  :bind (("C-c o a" . org-agenda)
         ("C-c o b" . org-switchb)
         ("C-c o c" . org-capture)
         ("C-c o g" . counsel-org-goto)
         ("C-c o G" . counsel-org-goto-all)
         ("C-c o l" . org-store-link)
         ("C-c o o" . org-open-at-point)
         ("C-c o p" . org-set-property)
         ("C-c o t" . org-set-tags)
         ("C-c o r" . csb/refile-with-ivy))
  :config
  ;; Do I like this being recursive?
  (setq csb/org-files (find-lisp-find-files  org-directory "\.org$"))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-done t)
  (setq org-todo-keywords '("TODO(t)" "WAITING(w)" "|"
                            "DONE(d)" "CANCELED(c)"))
  (setq org-agenda-files csb/org-files)
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-span 'fortnight)
  (setq org-deadline-warning-days 14)
  ;; The ivy/org interaction here is super wonky
  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks")
           "* TODO %?\n  %u\n  %a" :kill-buffer t)
          ("j" "Journal" entry (file+olp+datetree "" "Journal")
           "* %?\nEntered on %u\n  %i\n  %a" :kill-buffer t)
          ("n" "Note" entry (file+headline "" "Notes")
           "* %?\n  %u\n  %a" :kill-buffer t)
          ("u" "URL" entry (file+headline "" "URLs")
           "* %x%?\n  %u\n  %a" :kill-buffer t)))
  (setq org-refile-targets '((csb/org-files :maxlevel . 4))))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "✸" "◆" "⚫" "✿" "◇" "○" "★")))


;; "sidebar" outline for org-mode and anything that supports imenu
(use-package imenu-list
  :straight t
  :bind ([f9] . imenu-list-smart-toggle))


;; sql indenting
(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode))


;; dot files
(use-package graphviz-dot-mode
  :straight t
  :mode ("\\.dot$" . graphviz-dot-mode))

;; markdown-mode
(use-package markdown-mode
  :straight t
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
  :straight t
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
  :straight t
  :defer t)


;; nice commit messages
(use-package git-commit
  :straight t
  :hook (git-commit-mode . (lambda () (setq fill-column 70)))
  :config
  (setq git-commit-summary-max-length 50))


(use-package undo-tree
  :straight t
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
