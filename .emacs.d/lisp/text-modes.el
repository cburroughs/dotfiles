;; -*- lexical-binding: t -*-

;; Customization for text modes; org, LaTex and friends

(require 'use-package)


;; fill, width, writing
(setq-default fill-column 78)

;; http://emacshorrors.com/posts/longlines-mode.html
(use-package visual-fill-column
  :straight t
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))


;; -------------
;; org mode stuff


;; TODO: https://emacs.stackexchange.com/questions/7432/make-visual-line-mode-more-compatible-with-org-mode/12437
;; ^^ How can I get visual-line-mode/visual-fill-column mode to play nice with
;; org-mode. By not wrapping headings?


(use-package find-lisp)

(use-package org
  :straight nil ;; use gentoo package
  :mode ("\\.org\\'" . org-mode)
  :requires (find-lisp which-key)
  :init
  (which-key-add-key-based-replacements
    "C-c o" "org")
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
         ("C-c o r" . org-refile))
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


;; pseudo-successor to org-bullets
(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars t)
  (setq org-superstar-cycle-headline-bullets t)
  (setq org-superstar-prettify-item-bullets nil)
  (setq org-superstar-special-todo-items nil)
  (setq org-superstar-headline-bullets-list
        '("◉" "✸" "✻" "✿" "★" "◆" "🞛" "○" "◇"))) 



;; (use-package org-roam
;;   :straight t
;;   :defer 1
;;   :requires which-key
;;   :custom
;;   (org-roam-directory (file-truename "/tmp/roam"))
;;   (org-roam-completion-everywhere t)
;;   :bind (("C-c r l" . org-roam-buffer-toggle)
;;          ("C-c r f" . org-roam-node-find)
;;          ("C-c r g" . org-roam-graph)
;;          ("C-c r i" . org-roam-node-insert)
;;          ("C-c r c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c r j" . org-roam-dailies-capture-today))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more
;;   ;; informative completion interface
;;   (setq org-roam-node-display-template
;;         (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol)
;;   :init
;;     (which-key-add-key-based-replacements
;;       "C-c r" "org-roam")
;;   (setq org-roam-v2-ack t)
;;   (org-roam-db-autosync-mode))

;; (use-package org-roam-ui
;;   :straight t
;;   :after org-roam)


(use-package citar
  :straight t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/media/papers/library.bib"))
  (citar-library-paths '("~/media/papers/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))


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



;; full screen margins: Olivetti is a simple Emacs minor mode for a nice writing
;; environment.
(use-package olivetti
  :straight t
  :defer 1)

(defun prose-time ()
  "Full screen writing focus"
  (interactive)
  (olivetti-mode))




(provide 'csb/text-modes)
