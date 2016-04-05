;; Chris Burroughs
;; My dot emacs

;; todo: why does color-theme not fully load? (appear to loose colors on xwindow resize)
;; todo: figure out java-annotation support
;; todo: A happy round of commenting.
;; todo: Reduce the use of require (effective dot.emacs)
;; todo: use Alt-n to switch 'tabs'
;; todo/GOAL: less than 1s load time
;; todo: Make jdee work
;; todo: dictionary(?) integration (thesaurus would be super awesome too). http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; todo: func to resize to max screen width (across monitors)
;; todo: Do I like vi dot mode?
;; todo: What does pc-select etc mode not work in ubuntu land?
;; todo: more insert date options
;; todo: investigate ropemacs
;; todo: look into running emacsclient for terminal stuff
;; todo: open in firefox (new tab?)
;; todo: color-theme for org mode when opened directly
;; todo: add pycomplete to hippie-expand as well
;; todo: better way to set PYTHONPATH so gui launcher figures it out
;; todo: what to do about env not being sourced by gui launcher
;; todo: can tabbars be made to horizontally scroll?
;; todo: spell check not comments (ie for svn commit messages)
;; todo: document things I expect the package manager to install (partially done)
;; todo: http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
;; todo: http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html
;; todo: turn off hl-p in vc annotate mode
;; todo: get tags of some sort working, they seem awesome
;; todo: js-comint code review
;; todo: look into nXhtml
;; todo: why does haxe mode have recursive load errors when compiled?
;; todo: make it so color-theme and slime don't screw each other up
;; todo: http://www.emacswiki.org/emacs/SearchBuffers look here for replace
;; todo: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html , needs newer emacs snapshot?
;; todo: surround word with punctuation mode.  hitting ' in middle of word quotes it
;; todo: next time I am hacking lisp code I really should look at paredit
;; todo: integrate ecb with smart-split
;; todo: http://www.neilvandyke.org/quack/
;; todo: a simple trac wiki highlighting mode
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
;; todo: more scala goodies: http://scala.sygneca.com/tools/emacs
;; todo: look into more anything.el goodness
;; TODO: revisit snippets

; My .emacs "file".  All this stuff that has been accumulated and
; borrowed is probably under the GPL or public domain.  But to be
; sure, check the notice in each file.

; Goal: Runs under emacs-23 on gentoo and ubuntu

(require 'cl) ; TODO: Everyone says this is awesome, find out why

;; from https://www.emacswiki.org/emacs/OptimizingEmacsStartup
(defvar *emacs-load-start* (current-time))
(defun anarcat/time-to-ms (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time)))
	1000000) (car (cdr (cdr time)))))
(defun anarcat/display-timing ()
  (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms (current-time)) (anarcat/time-to-ms *emacs-load-start*)) 1000000.0)))
(add-hook 'after-init-hook 'anarcat/display-timing t)

;; For now I want things to work
(defun my-gentoo? ()
   (string-match "gentoo" (shell-command-to-string "uname -a")))

;; load gentoo installed stuff
;; assume if gentoo using gentoo kernel
(require 'site-gentoo nil t)

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))
			
(add-path "~/.emacs.d/lisp/")
(add-path "~/.emacs.d/site-lisp/")
(add-path "~/.emacs.d/site-lisp/scala-mode/")

(add-path "~/local_install/malabar/malabar-1.4-SNAPSHOT/lisp/")

;; It is easy enough to byte compile everything, so we might as well
;; The 0 option makes an .elc file even if one is not yet present
(byte-recompile-directory "~/.emacs.d/" 0)

;; If I ever use customize crap I don't want it pooping on this file
;; I assume this is automatically loaded
(setq custom-file "~/.emac.ds/.emacs-custom.el")
(load custom-file 'noerror)

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
;; http://snarfed.org/space/gnu%20emacs%20backup%20files
;; don't splatter backup and autosave files everywhere

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
;; but not in temp
(defvar autosave-dir
 (concat "~/.emacs_auto/autosaves/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "~/.emacs_auto/backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(setq semanticdb-default-save-directory "/tmp/")

(setq make-backup-files t)

;;; Dependencies

;; I expect the os package manager to take care of

;; app-emacs/color-theme
;; app-emacs/ecb +java
;; app-emacs/graphviz-dot-mode
;; app-emacs/jde
;; app-emacs/matlab
;; app-emacs/pymacs
;; app-emacs/ruby-mode
;; jde
;; app-emacs/crontab-mode 

;; I expect to be in in local_install

;; cedet

;;app-emacs/slime ; slime does not
;; have real releases so every machine will use a different
;; version. It's a crazy world. Least bad solution is to dump their
;; cvs tarball in a directory.

;; Other:
;; tidy
;; http://code.google.com/p/m2jdee/ <-- for maven/jde file generation
