;; Chris Burroughs
;; My dot emacs


;; load gentoo installed stuff
;; assume if gentoo using gentoo kernel
;(if (string-match "gentoo" (shell-command "uname -a")) ;; need to make it work
(load "/usr/share/emacs/site-lisp/site-gentoo")

;; Need to set up path for elisp files
(defun add-path (p)
  (add-to-list 'load-path p))
			
(add-path "~/emacs/")
(add-path "~/emacs/site-lisp/")

; load the files with the rest of my info
(load-library "key-bindings")
(load-library "pretty")
(load-library "modes")


;; --------------------------
;; Misc

;; change spelling
(setq-default ispell-program-name "aspell")

;; boo splash screens
(setq inhibit-startup-message t)

;; defaults to showing parens
(show-paren-mode t)

; awesome fonts only with emacs 23!
(if (string-match "23\." (emacs-version))
    (set-default-font "Bitstream Vera Sans Mono-10"))

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(set-scroll-bar-mode 'right)

(setq frame-title-format '(buffer-file-name "%b -- %f" ( "%b" ) ) )

;; from steve yegge
(defun wc-w ()
  "Print stats on current blog column, or blogollum, or whatever"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((char-count 0))
      (while (not (eobp))
        (unless (looking-at "[ \t\r\n]")
          (incf char-count))
        (forward-char 1))
      (message "%d chars, %d words" char-count (/ char-count 5)))))



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
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(setq semanticdb-default-save-directory "/tmp/")

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)
; would like to be able to use shift- arrow to change date


;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn 	 
          (rename-file name new-name 1) 	 
          (rename-buffer new-name) 	 
          (set-visited-file-name new-name) 	 
          (set-buffer-modified-p nil))))))