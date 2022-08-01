;; -*- lexical-binding: t -*-

;; General Custom keybindings

(require 'use-package)

;; Item 2: Invoke M-x without the Alt key
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; Item 9: Master Emacs's regular expressions
(defalias 'qrr 'query-replace-regexp)

;; Need to force this some times?
(global-set-key (kbd "C-c C-c") 'comment-region)

;; cua-like
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)


;; almost-vi/ergo
;;              i      =          C-p
;;     M-     j k l    =    C-b   C-n   C-f

(require 'ergo-movement-mode)
(ergo-movement-mode 1)


;; http://pragmaticemacs.com/emacs/scrolling-and-moving-by-line/
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-up-command 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-down-command 1)))

;; spell checking
(global-set-key [f7] 'ispell-buffer)
(global-set-key [Shift-f7] 'ispell-comments-and-strings)

;; hey the keyboard does say open on that key
(global-set-key [f5] 'browse-url-at-point)

;; esk: Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; esk: Indentation help
(global-set-key (kbd "C-x ^") 'join-line)


;; rectangular selection
;; http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)
(global-set-key (kbd "C-x r <down-mouse-1>") 'rm-mouse-drag-region)

;; slightly better buffer listing ("Better Defaults")
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Delete upt to the occurance of a character
(global-set-key (kbd "M-z") 'zap-up-to-char)



;; Fromhttps://github.com/ergoemacs/ergoemacs-mode/blob/8ea6d54c7d576da0ebdf44e11c7398859f8dc834/ergoemacs-functions.el

(defvar csb/ergoemacs-user-buffer-functions nil
  "A user buffer is one whose name does not start with *, or for which
any of the functions in `csb/ergoemacs-user-buffer-functions' returns
true; otherwise it is an emacs buffer. Each function takes a buffer as
argument.")

(defun csb/ergoemacs-user-buffer-p (buffer)
  "Is BUFFER a user buffer?
A user buffer is one whose name does not start with *, or for which
any of the functions in `csb/ergoemacs-user-buffer-functions' returns
true; otherwise it is an emacs buffer."
  (or
   (not (string= "*" (substring (buffer-name buffer) 0 1)))
   (cl-some
    (lambda (func)
      (funcall func buffer))
    csb/ergoemacs-user-buffer-functions)))

(defun csb/ergoemacs-change-buffer (&optional number previous-buffer-p emacs-buffer-p)
  "Switch to the next/previous emacs/user buffer.
By default this switches to the next user buffer.
This function switches forward/backward NUMBER emacs/user buffers.
This can be specified by a prefix argument.
When PREVIOUS-BUFFER-P is non-nil, switch to a previous buffer.
When EMACS-BUFFER-P is non-nil switch to an emacs buffer.  A user
buffer is one whose name does not start with *, or for which any of
the functions in `csb/ergoemacs-user-buffer-functions' returns true;
otherwise it is an emacs buffer."
  (interactive)
  (let ((curr-buffer (current-buffer))
        (number (or (and number (abs number)) 1))
        (i 0))
    (while (< i number)
      (if previous-buffer-p
          (previous-buffer)
        (next-buffer))
      (while (and (or (and (not emacs-buffer-p)
                           (not (csb/ergoemacs-user-buffer-p (current-buffer))))
                      (and emacs-buffer-p
                           (csb/ergoemacs-user-buffer-p (current-buffer))))
                  (not (eq curr-buffer (current-buffer))))
        (if previous-buffer-p
            (previous-buffer)
          (next-buffer)))
      (when (and (eq curr-buffer (current-buffer)) (= i 0))
        (if emacs-buffer-p
            (message "Could not find any %semacs buffers."
                     (or (and (not (csb/ergoemacs-user-buffer-p (current-buffer))) "other ") ""))
          (message "Could not find any %suser buffers."
                   (or (and (csb/ergoemacs-user-buffer-p (current-buffer)) "other ") "")))
        (setq i (+ number 1)))
      (setq i (+ 1 i)))))

(defun csb/ergoemacs-next-user-buffer (&optional number)
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive "p")
  (csb/ergoemacs-change-buffer number))

(defun csb/ergoemacs-previous-user-buffer (&optional number)
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive "p")
  (csb/ergoemacs-change-buffer number t))

(defun csb/ergoemacs-next-emacs-buffer (&optional number)
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive "p")
  (csb/ergoemacs-change-buffer number nil t))

(defun csb/ergoemacs-previous-emacs-buffer (&optional number)
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive "p")
  (csb/ergoemacs-change-buffer number t t))

(global-set-key (kbd "<C-prior>") 'csb/ergoemacs-previous-user-buffer)
(global-set-key (kbd "<C-next>") 'csb/ergoemacs-next-user-buffer)
(global-set-key (kbd "<C-S-prior>") 'csb/ergoemacs-previous-emacs-buffer)
(global-set-key (kbd "<C-S-next>") 'csb/ergoemacs-next-emacs-buffer)


;; https://github.com/radian-software/ctrlf/issues/112
;; https://github.com/radian-software/radian/blob/2d3a1f8c62ce4804bda94f26d7f70674bec941bf/emacs/radian.el#L1029-L1051
(defun csb/advice-keyboard-quit-minibuffer-first (orig-fn &rest args)
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (progn
        (switch-to-buffer (window-buffer minibuffer))
        (cond
         ((featurep 'delsel)
          (progn
            (eval-when-compile
              (require 'delsel))
            (minibuffer-keyboard-quit)))
         ;; Emacs 28 and later
         ((fboundp 'abort-minibuffers)
          (abort-minibuffers))
         ;; Emacs 27 and earlier
         (t
          (abort-recursive-edit))))
    (apply orig-fn args)))

(advice-add 'keyboard-quit :around #'csb/advice-keyboard-quit-minibuffer-first)



;; meta

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.1)
  (setq which-key-compute-remaps t)
  (which-key-mode 1))


(provide 'csb/keybindings)
