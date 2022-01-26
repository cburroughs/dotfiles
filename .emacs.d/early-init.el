;; -*- lexical-binding: t -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; More timing details adapted from doom
(defvar csb/init/init-time nil)

;; from https://www.emacswiki.org/emacs/OptimizingEmacsStartup
(add-hook 'window-setup-hook
          (lambda ()
            (progn
              (setq csb/init/init-time
                    (float-time (time-subtract (current-time) before-init-time)))
              (message
               "window-setup-hook-time: %.3f seconds (gc num:%i time:%f)"
               csb/init/init-time gcs-done gc-elapsed))))


;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq csb/init/default-gc-threshold gc-cons-threshold
      csb/init/default-gc-cons-percentage gc-cons-percentage
      csb/init/gc-cons-normal-threshold (* 1024 1024)
      csb/init/gc-cons-large-threshold (* 64 1024 1024))

(setq gc-cons-threshold csb/init/gc-cons-large-threshold)
(setq gc-cons-percentage 0.9)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold csb/init/gc-cons-normal-threshold
                  gc-cons-percentage csb/init/default-gc-cons-percentage)))
(add-hook 'after-init-hook
          (lambda ()
             (message "init gc num:%i time:%f" gcs-done gc-elapsed)))
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold csb/init/gc-cons-large-threshold)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold csb/init/gc-cons-normal-threshold)))
(add-function :after after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

(defvar csb/init/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist csb/init/file-name-handler-alist)))


;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)



;; More Magic from DOOM

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
