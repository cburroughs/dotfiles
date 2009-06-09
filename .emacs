;; Chris Burroughs
;; My dot emacs

;; TODO: why does color-theme not fully load? (appear to loose colors on xwindow resize)
;; TODO: figure out java-annotation support
;; TODO: A happy round of commenting.
;; TODO: Reduce the use of require (effective dot.emacs)
;; TODO: use Alt-n to switch 'tabs'
;; TODO/GOAL: less than 1s load time
;; TODO: Make jdee work
;; TODO: dictionary(?) integration (thesaurus would be super awesome too). http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; TODO: func to resize to max screen width (across monitors)
;; TODO: Do I like vi dot mode?
;; TODO: What does pc-select etc mode not work in ubuntu land?
;; TODO: more insert date options
;; TODO: investigate ropemacs
;; TODO: look into running emacsclient for terminal stuff
;; TODO: open in firefox (new tab?)
;; TODO: color-theme for org mode when opened directly
;; TODO: add pycomplete to hippie-expand as well
;; TODO: better way to set PYTHONPATH so gui launcher figures it out
;; TODO: what to do about env not being sourced by gui launcher
;; TODO: can tabbars be made to horizontally scroll?
;; TODO: spell check not comments (ie for svn commit messages)
;; TODO: document things I expect the package manager to install (partially done)
;; todo: http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
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

; My .emacs "file".  All this stuff that has been accumulated and
; borrowed is probably under the GPL or public domain.  But to be
; sure, check the notice in each file.

; Goal: Runs under emacs-23 on gentoo and ubuntu
; TODO: There is no good reason for it not to work in emacs 22

(require 'cl) ; TODO: Everyone says this is awesome, find out why

;; See http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time)) ; Find out how long this takes
;; Progress:
;; Originally: 3s
;; Pre-compiled: 2s

;; For now I want things to work
(defun my-gentoo? ()
   (string-match "gentoo" (shell-command-to-string "uname -a")))

;; load gentoo installed stuff
;; assume if gentoo using gentoo kernel
(require 'site-gentoo nil t)

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))
			
(add-path "~/emacs/")
(add-path "~/emacs/site-lisp/")

;; It is easy enough to byte compile everything, so we might as well
;; The 0 option makes an .elc file even if one is not yet present
(byte-recompile-directory "~/emacs/" 0)
(shell-command "rm ~/emacs/site-lisp/haxe-mode.elc")


;; If I ever use customize crap I don't want it pooping on this file
;; I assume this is automatically loaded
(setq custom-file "~/emacs/.emacs-custom.el")
(load custom-file 'noerror)

;; load the files with the rest of my info
;; try to put in order of least likely to break
(load-library "key-bindings")
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
 (concat "~/.emacs_autosaves/" (user-login-name) "/"))

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
(defvar backup-dir (concat "/tmp/" (user-login-name) "/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(setq semanticdb-default-save-directory "/tmp/")

(setq make-backup-files t)


;; end -- so we measure the time here
(message "My .emacs loaded in %ds" 
         (destructuring-bind (hi lo ms) (current-time) 
           (- (+ hi lo) (+ (first *emacs-load-start*) 
                           (second *emacs-load-start*)))))


;; Dependencies I expect the os package manager to take care of

;; app-emacs/cedet
;; app-emacs/color-theme
;; app-emacs/ecb +java
;; app-emacs/graphviz-dot-mode
;; app-emacs/jde
;; app-emacs/matlab
;; app-emacs/pymacs
;; app-emacs/ruby-mode
;; app-emacs/slime

;; Other:
;; tidy
