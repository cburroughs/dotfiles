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
  (setq csb/main-org-directory "~/Documents/org")
  ;; Used for default relative path for capture templates and agenda
  (setq org-directory csb/main-org-directory)
  (setq csb/org-files (find-lisp-find-files  csb/main-org-directory "\.org$"))
  (setq org-default-notes-file (concat csb/main-org-directory "/inbox.org"))
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
           "* %x%?\n  %u\n  %a" :kill-buffer t)
          ("z" "Zettel (slipbox notes)" entry
           (file+headline "" "Zettel (slipbox) notes")
           "* %?\n  %u\n  %a" :kill-buffer t)))
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



(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/Documents/zettelkasten"))
  (org-roam-db-location "~/.config/emacs/org-roam.db")
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold csb/init/gc-cons-large-threshold)
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)
         ;; custom
         ("C-c r c" . csb/org-roam-node-from-cite))
  :config
  ;; The author’s recommended configuration is as follows: Crucially, the window
  ;; is a regular window (not a side-window), and this allows for predictable
  ;; navigation:
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  ;; from https://jethrokuan.github.io/org-roam-guide/
  (defun csb/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
#+title: ${title}
[cite:@${citekey}]\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))
  ;; from https://jethrokuan.github.io/org-roam-guide/
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; from https://jethrokuan.github.io/org-roam-guide/
  (defun csb/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'csb/tag-new-node-as-draft)
  ;; If you're using a vertical completion framework, you might want a more
  ;; informative completion interface
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (which-key-add-key-based-replacements
    "C-c r" "org-roam"))

(use-package org-roam-ui
  :straight t
  :after org-roam)


(use-package citar
  :straight t
  :commands citar-select-ref
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
  :defer 1
  :config
  (defun prose-time ()
    "Full screen writing focus"
    (interactive)
    (olivetti-mode)))



;;; futzing with pdfs, which are almost like text

(use-package pdf-tools
  :straight t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query)
  :init
   ;; control-f package doesn't work for some reason
  (add-hook 'pdf-view-mode-hook
            (lambda () (ctrlf-local-mode -1))))


(provide 'csb/text-modes)
