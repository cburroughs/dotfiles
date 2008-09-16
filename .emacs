;; Chris Burroughs
;; My dot emacs

; TODO: why does color-theme not fully load?
; TODO: figure out java-annotation support
; TODO: rpol xml minor mode
; TODO: A happy round of commenting.
; TODO: Reduce the use of require (effictive dot.emacs)
; TODO: use Alt-n to switch 'tabs'
; TODO/GOAL: less than 1s load time
; TODO: Make jdee work
; TODO: evaluate ecb
; TODO: func to resize to max screen width (across monitors)
; TODO: Do I live vi dot mode?

; My .emacs "file".  All this stuff that has acumilated and borrowed
; is probably under the GPL or public domian.

; Goal: Runs under emacs-23 on gentoo and ubuntu
; TODO: There is no good reason for it not to work in emacs 22

(require 'cl) ; TODO: Everyone says this is awesome, find out why

;; See http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time)) ; Find out how long this takes
; Progress:
; Originally: 3s
; Pre-compiled: 2s

; For now I want things to work
(defun my-gentoo? ()
   (string-match "gentoo" (shell-command-to-string "uname -a")))

;; load gentoo installed stuff
;; assume if gentoo using gentoo kernel
(when (my-gentoo?)
    (load "/usr/share/emacs/site-lisp/site-gentoo"))

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))
			
(add-path "~/emacs/")
(add-path "~/emacs/site-lisp/")

; It is easy enough to byte compile everything, so we might as well
; The 0 option makes an .elc file even if one is not yet present
(byte-recompile-directory "~/emacs/" 0)



; If I ever use customize crap I don't want it pooping on this file
; I assume this is automatically loaded
(setq custom-file "~/emacs/.emacs-custom.el")
(load custom-file 'noerror)

; load the files with the rest of my info
; try ti put in order of least likely to break
(load-library "key-bindings")
(load-library "efuncs")
(load-library "misc")
(load-library "pretty")
(load-library "modes")

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