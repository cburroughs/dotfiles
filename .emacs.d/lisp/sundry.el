;; -*- lexical-binding: t -*-

;; misc changes and variable settings



;; make the builtin ibuffer list nicer
(add-hook 'ibuffer-hook '(lambda ()
                           (setq ibuffer-show-empty-filter-groups nil)
	                       (ibuffer-auto-mode 1)
                           (unless (eq ibuffer-sorting-mode 'alphabetic)
                             (ibuffer-do-sort-by-alphabetic))))


(use-package all-the-icons-ibuffer
  :straight t
  :init
  (setq all-the-icons-ibuffer-human-readable-size t)
  (all-the-icons-ibuffer-mode 1))


(use-package ibuffer-projectile
  :straight t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    (append (ibuffer-projectile-generate-filter-groups)
	                        '(("Files" (and (filename . ".*")
                                            (not (name . "^\\*"))))
		                      ("Planner" (or
				                          (name . "^\\*Calendar\\*$")
				                          (name . "^\\*Org Agenda\\*")))
                              ("shell" (or (mode . eshell-mode)
                                           (mode . shell-mode)
                                           (derived-mode . term-mode)))
		                      ("dired" (mode . dired-mode))
		                      ("magit" (name . "^magit"))
                              ("Help" (or (name . "\*Help\*")
		                                  (name . "\*Apropos\*")
		                                  (name . "\*info\*")))
                              ("emacs" (name . "^\\*"))))))))


;; Appears to now work with emacs > 26 https://github.com/jschaf/esup/issues/54
;;(use-package esup
;;  :ensure t
;;  :defer 1)

;; from gentoo site-package
(use-package with-editor
  :init
  (if (csb/gentoo-emacs-vcs-p)
      (setq with-editor-emacsclient-executable
            (concat "emacsclient-emacs-"
                    (nth 0 (split-string emacs-version "\\."))
                    "-vcs"))))

;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; copy-paste should work with other X clients
;; duh; only do this when you have x
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; change spelling
(setq-default ispell-program-name "aspell")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; graphviz dot mode requires this to be set, unclear how it is different from the above
(setq-default default-tab-width 4)


;; smooth scrolling
(setq scroll-conservatively 1000)
(setq scroll-preserve-screen-position t)



;; Try to avoid giving buffers dumb names
(use-package uniquify
  :defer 1
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;; default to firefox in new tab
(setq browse-url-default-browser 'browse-url-firefox-program)
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab 't) ; why does this not work?


(use-package projectile
  :straight t
  :config
  (setq projectile-known-projects-file "~/.config/emacs/projectile-bookmarks.el")
  (setq projectile-completion-system 'auto)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "build/")
  (add-to-list 'projectile-globally-ignored-directories "dist/")
  (add-to-list 'projectile-globally-ignored-directories "target/")
  (projectile-register-project-type 'pants '("pants.ini"))
  (projectile-mode +1))


(autoload 'columnize-text "columnize"
  "Formats a list of items into columns (pillars)" t)

;; http://www.emacswiki.org/emacs/FillParagraph
;; http://jblevins.org/projects/markdown-mode/
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.\\|[ \t]*: ")


(use-package lorem-ipsum
  :straight t
  :defer 2)

(use-package neotree
  :straight t
  :defer 1
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-autorefresh nil))


;; http://pragmaticemacs.com/emacs/world-clock-in-emacs/
;; for display-time-world
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Los_Angeles")
        ("America/New_York" "New York")
        ("America/Vancouver" "Vancouver")
        ("Europe/Berlin" "Berlin")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Asia/Seoul" "Seoul")
        ("Asia/Shanghai" "Bejing/Shanghai")
        ("Asia/Tokyo" "Tokyo")))





;;; Windows and jumping around

;; "undo" for window changes
(when (fboundp 'winner-mode)
  (winner-mode 1))

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))


;; Do I actually end up using this?
(use-package avy
  :straight t
  :bind (("C-;" . avy-goto-char-2)
         ("C-'" . avy-goto-line))
  :config (setq avy-all-windows 't))


;; Do I actually end up using this?
(use-package eyebrowse
  :straight t
  :defer 1
  :config
  (eyebrowse-mode t))
