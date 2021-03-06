;; Chris Burroughs
;; My dot emacs

;; todo: see if esup is working
;; todo: dictionary(?) integration (thesaurus would be super awesome too). http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; todo: more insert date options
;; todo: look into running emacsclient for terminal stuff
;; todo: open in firefox (new tab?)
;; todo: spell check not comments (ie for svn commit messages)
;; todo: http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
;; todo: http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html
;; todo: surround word with punctuation mode.  hitting ' in middle of word quotes it
;; todo: next time I am hacking lisp code I really should look at paredit
;; todo: http://www.neilvandyke.org/quack/
;; todo: http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/
;; todo: http://linuxcommando.blogspot.com/2008/05/root-edit-file-using-emacs-in-same.html
;; todo: http://emacsworld.blogspot.com/2009/11/quick-way-of-looking-up-colours-in.html , also html colors?
;; todo: Textmate ExecuteExternalCommand bridge
;; todo: is there an easy way to switch all modes to 2 or 4 sapce indent?
;; TODO: revisit snippets
;; TODO: Untangle  https://www.emacswiki.org/emacs/GitCommitMode
;; TODO: https://github.com/alpaker/Fill-Column-Indicator
;; TODO revisit shell usage https://manuel-uberti.github.io/emacs/2017/10/07/m-x-shell/
;; TODO: Look into defining some abbreviations http://ergoemacs.org/emacs/emacs_abbrev_mode.html
;; TODO: Look into hydra again & https://gitlab.com/jjzmajic/hercules.el
;; TODO: revisit lsp-java
;; TODO: Consider elgot vs lsp-mode again
;; TODO Chck out https://github.com/cireu/counsel-ffdata
;; TODO: https://github.com/Fuco1/free-keys and bind-keys

;; My .emacs "file".  All this stuff that has been accumulated and
;; borrowed is probably under the GPL or public domain.  But to be
;; sure, check the notice in each file.


;; More timing details adapted from doom
(defvar csb/init/init-time nil)


;; from https://www.emacswiki.org/emacs/OptimizingEmacsStartup
(add-hook 'window-setup-hook
          (lambda () (progn
                       (setq csb/init/init-time (float-time (time-subtract (current-time) before-init-time)))
                      (message "emacs-init-time: %.3f seconds" csb/init/init-time))))


;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq csb/init/gc-cons-normal-threshold (* 1024 1024))
(setq csb/init/gc-cons-large-threshold (* 48 1024 1024))
(setq csb/init/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold csb/init/gc-cons-large-threshold)
(setq gc-cons-percentage 0.9)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold csb/init/gc-cons-normal-threshold
                           gc-cons-percentage csb/init/gc-cons-percentage)))
(add-hook 'after-init-hook
          (lambda ()
             (message "init gc num:%i time:%f" gcs-done gc-elapsed)))
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold csb/init/gc-cons-large-threshold)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold csb/init/gc-cons-normal-threshold)))
(add-hook 'focus-out-hook 'garbage-collect)

(defvar csb/init/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist csb/init/file-name-handler-alist)))


;; Something will end up wanting it eventually
(require 'cl)

;; load gentoo installed stuff
(require 'site-gentoo nil t)

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))

(add-path "~/.emacs.d/lisp/")
(add-path "~/.emacs.d/site-lisp/")


;; "Setting load-prefer-newer prevents stale elisp bytecode from shadowing more
;; up-to-date source files." (Better Defaults)
(setq load-prefer-newer t)


;; It is easy enough to byte compile everything, so we might as well
;; The 0 option makes an .elc file even if one is not yet present
(byte-recompile-directory "~/.emacs.d/site-lisp" 0)
(byte-recompile-directory "~/.emacs.d/lisp")

;; If I ever use customize crap I don't want it pooping on this file
;; I assume this is automatically loaded
(setq custom-file "~/.config/emacs/.emacs-custom.el")
(load custom-file 'noerror)


;; Bootstrap pkgs http://cachestocaches.com/2015/8/getting-started-use-package/
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package dash :ensure t)
(use-package diminish :ensure t)

;; --------------------------
;; Putting Files places

;; copied from the internet
;; https://snarfed.org/gnu_emacs_backup_files
;; http://pragmaticemacs.com/emacs/auto-save-and-backup-every-save/
;; don't splatter backup and autosave files everywhere

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the file
;; system, not in /tmp where they can be lost when most needed

(defvar csb-auto-save-dir "~/.config/emacs/autosaves/")

(make-directory csb-auto-save-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat csb-auto-save-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))


;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar csb-backup-dir (concat "~/.config/emacs/backups/"))
(setq backup-directory-alist (list (cons "." csb-backup-dir)))
(setq
 backup-by-copying t     ; don't clobber symlinks
 kept-new-versions 10    ; keep 10 latest versions
 kept-old-versions 0     ; don't bother with old versions
 delete-old-versions t   ; don't ask about deleting old versions
 version-control t       ; number backups
 vc-make-backup-files t) ; backup version controlled files

(setq make-backup-files t)


;; Save a list of recent files visited.
(setq recentf-max-saved-items 128
      recentf-max-menu-items 24)
(setq recentf-save-file (expand-file-name "~/.config/emacs/recentf"))
(recentf-mode 1)


;; Is this still relevant?
(setq semanticdb-default-save-directory "/tmp/")

;; https://www.emacswiki.org/emacs/SavePlace
(use-package save-place-mode
  :defer 1
  :config
  (setq save-place-file "~/.config/emacs/places/emacs-places")
  (setq save-place-forget-unreadable-files nil)
  (save-place-mode 1))


;; --------------------------
;; load the files with the rest of my info
;; try to put in order of least likely to break
(load-library "keybindings")
(load-library "efuncs")
(load-library "sundry")
(load-library "palette")
(load-library "pretty")
(load-library "modes")
(load-library "prog-modes")
(load-library "external")


;; fin
(garbage-collect)

;;; Dependencies

;; I expect the os package manager to take care of

;; app-emacs/crontab-mode
;; app-emacs/ecb +java
;; app-emacs/graphviz-dot-mode
;; app-emacs/jde
;; app-emacs/matlab
;; app-emacs/ruby-mode
;; app-emacs/yaml-mode

;; External:
;; dev-vcs/git
;; sys-apps/ripgrep
