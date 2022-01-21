;; -*- lexical-binding: t -*-

;; Chris Burroughs
;; My dot emacs

;; todo: dictionary(?) integration (thesaurus would be super awesome too). http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; todo: more insert date options
;; todo: open in firefox (new tab?)
;; todo: http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
;; todo: http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html
;; todo: surround word with punctuation mode.  hitting ' in middle of word quotes it
;; todo: http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/
;; todo: http://linuxcommando.blogspot.com/2008/05/root-edit-file-using-emacs-in-same.html
;; todo: http://emacsworld.blogspot.com/2009/11/quick-way-of-looking-up-colours-in.html , also html colors?
;; todo: Textmate ExecuteExternalCommand bridge
;; TODO: revisit snippets
;; TODO revisit shell usage https://manuel-uberti.github.io/emacs/2017/10/07/m-x-shell/
;; TODO: Look into defining some abbreviations http://ergoemacs.org/emacs/emacs_abbrev_mode.html
;; TODO: Look into hydra again & https://gitlab.com/jjzmajic/hercules.el
;; TODO Chck out https://github.com/cireu/counsel-ffdata
;; TODO: https://github.com/Fuco1/free-keys and bind-keys

;; My .emacs "file".  All this stuff that has been accumulated and
;; borrowed is probably under the GPL or public domain.  But to be
;; sure, check the notice in each file.


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


;; straight.el
(setq straight-profiles `((nil .  "~/.emacs.d/straight.lockfile.el")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use package! without the bootstrap
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package dash :straight t)
(use-package diminish :straight t)

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
(load-library "completing")
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

;; app-emacs/apache-mode
;; app-emacs/color-theme
;; app-emacs/crontab-mode
;; app-emacs/ebuild-mode
;; app-emacs/graphviz-dot-mode
;; app-emacs/jde
;; app-emacs/lua-mode
;; app-emacs/matlab
;; app-emacs/org-mode
;; app-emacs/ruby-mode
;; app-emacs/rust-mode
;; app-emacs/with-editor
;; app-emacs/yaml-mode
;; app-emacs/zenburn

;; External:
;; dev-vcs/git
;; sys-apps/ripgrep
