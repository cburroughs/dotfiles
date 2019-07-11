;; Chris Burroughs
;; My dot emacs

;; todo: see if esup is working
;; todo: figure out java-annotation support
;; todo: Reduce the use of require (effective dot.emacs)
;; todo: use Alt-n to switch 'tabs'
;; todo: Switch tabbar switching behavior to not conflict with org-mode
;; todo: Make jdee work
;; todo: dictionary(?) integration (thesaurus would be super awesome too). http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; todo: func to resize to max screen width (across monitors)
;; todo: more insert date options
;; todo: investigate ropemacs
;; todo: look into running emacsclient for terminal stuff
;; todo: open in firefox (new tab?)
;; todo: add pycomplete to hippie-expand as well
;; todo: can tabbars be made to horizontally scroll?
;; todo: spell check not comments (ie for svn commit messages)
;; todo: document things I expect the package manager to install (partially done)
;; todo: http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
;; todo: http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html
;; todo: turn off hl-p in vc annotate mode
;; todo: get tags of some sort working, they seem awesome
;; todo: look into nXhtml
;; todo: http://www.emacswiki.org/emacs/SearchBuffers look here for replace
;; todo: surround word with punctuation mode.  hitting ' in middle of word quotes it
;; todo: next time I am hacking lisp code I really should look at paredit
;; todo: http://www.neilvandyke.org/quack/
;; todo: http://edward.oconnor.cx/elisp/hl-sexp.el
;; todo: http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/
;; todo: eldoc mode
;; todo: http://www.emacswiki.org/emacs/PrettyLambda
;; todo: http://linuxcommando.blogspot.com/2008/05/root-edit-file-using-emacs-in-same.html
;; todo: look at this: http://taesoo.org/Opensource/Pylookup
;; todo: http://emacsworld.blogspot.com/2009/11/quick-way-of-looking-up-colours-in.html , also html colors?
;; todo: visual C-x o
;; todo: Textmate ExecuteExternalCommand bridge
;; todo: is there an easy way to switch all modes to 2 or 4 sapce indent?
;; todo: look into more anything.el goodness
;; TODO: revisit snippets
;; TODO: Untangle  https://www.emacswiki.org/emacs/GitCommitMode
;; TODO: https://github.com/alpaker/Fill-Column-Indicator
;; TODO: Is there a unified "sidebar" that can do files, functions, org-outlines, etc?

;; My .emacs "file".  All this stuff that has been accumulated and
;; borrowed is probably under the GPL or public domain.  But to be
;; sure, check the notice in each file.

(require 'cl)

;; from https://www.emacswiki.org/emacs/OptimizingEmacsStartup
(add-hook 'after-init-hook
          (lambda () (message "emacs-init-time: %s" (emacs-init-time))))



;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq csb/init/gc-cons-large-threshold (* 24 1024 1024))
(setq csb/init/gc-cons-normal-threshold (* 1024 1024))
(setq gc-cons-threshold csb/init/gc-cons-large-threshold)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold
                           csb/init/gc-cons-normal-threshold)))
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


;; load gentoo installed stuff
(require 'site-gentoo nil t)

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))

(add-path "~/.emacs.d/lisp/")
(add-path "~/.emacs.d/site-lisp/")

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
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)


;; load the files with the rest of my info
;; try to put in order of least likely to break
(load-library "keybindings")
(load-library "efuncs")
(load-library "misc")
(load-library "pretty")
(load-library "modes")
(load-library "prog-modes")
(load-library "external")

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


;; Is this still relevant?
(setq semanticdb-default-save-directory "/tmp/")

;; fin
(garbage-collect)

;;; Dependencies

;; I expect the os package manager to take care of

;; app-emacs/ecb +java
;; app-emacs/graphviz-dot-mode
;; app-emacs/jde
;; app-emacs/matlab
;; app-emacs/ruby-mode
;; jde
;; app-emacs/crontab-mode
;; app-emacs/yaml-mode

;; I expect to be in in local_install

;; Other:
;; http://code.google.com/p/m2jdee/ <-- for maven/jde file generation
