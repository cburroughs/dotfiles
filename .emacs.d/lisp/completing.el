;; -*- lexical-binding: t -*-

;; fancy completion and (many) friends

(require 'use-package)

;; Hippie 💓💓💓 so simple; so often better than fancy alternatives

;; prefer hippie-expand over deavrez
(global-set-key (kbd "M-/") 'hippie-expand)

;; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;;;;  completing-read ;;;;


;; Minibuffer and Completions in Tandem
(use-package mct
  :straight t
  :init
  (setq mct-hide-completion-mode-line t)
  (setq mct-completion-passlist '(switch-to-buffer))
  (mct-minibuffer-mode 1))


(use-package marginalia
  :straight t
  :bind ((:map minibuffer-local-map
               ("M-A" . marginalia-cycle)))
  :init
  (marginalia-mode))


(use-package orderless
  :straight t
  ;; SPC should never complete: use it for `orderless' groups.
  :bind ((:map minibuffer-local-completion-map)
         ("SPC" . nil)
         ("?" . nil))
  :init
  ;; based on prot
  ;; See also https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
  (setq completion-styles '(basic substring partial-completion orderless))
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless))))))



;; NOTE: consult has *many* more completing-read commands to consider
(use-package consult
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . csb/consult-outline-or-org-heading)   ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         :map isearch-mode-map
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (defun csb/consult-outline-or-org-heading (&rest args)
    " Call org specific funtion in that mode, otherwise call the
more generic outline version"
    (interactive)
    (if (eq major-mode 'org-mode)
        (apply 'consult-org-heading args)
      (apply 'consult-outline args)))
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)


;; Switch betwen action->object and object->action among completion objects
;; Literal paradigm switch
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))


(use-package embark-consult
  :straight t
  :after (embark consult))


;;;; completion-in-region ;;;;


(use-package corfu
  :straight t
  :after which-key
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  :bind (("C-c c p" . completion-at-point)) ;; capf
  :init
  (which-key-add-key-based-replacements
    "C-c c" "capfs")
  (corfu-global-mode))


;; for kind-icon
(use-package svg-lib
  :straight t
  :config
  (setq svg-lib-icons-dir "~/.config/emacs/.cache/svg-lib/"))


(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  :bind (("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-symbol)
         ("C-c c a" . cape-abbrev)
         ("C-c c i" . cape-ispell)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict)
         ("C-c c \\" . cape-tex)
         ("C-c c _" . cape-tex)
         ("C-c c ^" . cape-tex)
         ("C-c c &" . cape-sgml)
         ("C-c c r" . cape-rfc1345))
  :custom
  (cape-dict-file "/usr/share/dict/words")
  :init
  ;; from wiki
  ;; and https://takeonrules.com/2022/01/17/switching-from-company-to-corfu-for-emacs-completion/
  (defun csb/corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map (kbd "M-m") #'csbcorfu-move-to-minibuffer)

  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(provide 'csb/completing)
