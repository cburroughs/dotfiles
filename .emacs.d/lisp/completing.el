;; -*- lexical-binding: t -*-

;; fancy completion and (many) friends


;; NOTE: Neither ivy nor counsel are "enabled" to take over standard buffer or
;; file operations.  But a number of other handy functions are enabled.
(use-package ivy
  :straight t
  :demand t
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 0))


(use-package swiper
  :straight t
  :after ivy
  ;; https://oremacs.com/2019/04/07/swiper-isearch/ Maybe revsit fully switching
  ;; to swiper instead of isearch in the future?  Sadly seems prone to 100% cpu
  ;; hangs.  https://github.com/abo-abo/swiper/issues/925 is perhaps a relevant
  ;; issue
  :bind (("M-s" . swiper)
         ("M-r" . swiper-backward)
         ;;("M-s" . isearch-forward)
         ;;("M-r" . isearch-backward)
         ;; https://oremacs.com/2016/07/29/brand-new-swiper-all/
         ("C-c s" . swiper-all))
  :config
  (setq swiper-stay-on-quit nil))


(use-package counsel
  :straight t
  :after ivy
  :demand t
  ;; remap only some standard keybindings; add new ones
  :bind (("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f1> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep))
  :config
  ;; Why does this have to be defined here and not above?
  (global-set-key (kbd "C-c C-j") 'counsel-imenu)
  ;; Provisional, unclear on side effects
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

