;; -*- lexical-binding: t -*-

;; Functions and such

(require 'cl-lib)

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)
;; would like to be able to use shift- arrow to change date (like in org-mode)


;; steve yegge
;; Never understood why Emacs doesn't have this function.
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


(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)1)
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn 	(copy-file filename newname 1)
                (delete-file filename)
                (set-visited-file-name newname)
                (set-buffer-modified-p nil) 	t))))


;; NOTE: Emacs now (since 24.1) has built in support count-words and
;; count-words-region
(defun wc-w ()
  "Print stats on current blog column, or blogollum, or whatever"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((char-count 0))
      (while (not (eobp))
        (unless (looking-at "[ \t\r\n]")
          (cl-incf char-count))
        (forward-char 1))
      (message "%d chars, %d words" char-count (/ char-count 5)))))


;; convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))


(global-set-key [f11] 'toggle-frame-fullscreen)

(defun totally-fullscreen ()
  (interactive)
  ;; unclear why this toggle is not working in commands
  (if menu-bar-mode
      (menu-bar-mode -1)
    (menu-bar-mode 1))
  ;; (if tool-bar-mode
  ;;     (tool-bar-mode -1)
  ;;   (tool-bar-mode 1))
  (toggle-frame-fullscreen))

(global-set-key [f12] 'totally-fullscreen)


;; TODO: Investigate these
;; http://dotfiles.org/~coder_/.emacs
;; {{{ Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An interactive version of the query-replace-regexp function  that lets you
;; see your regexp search before executing it (Safety first!)
(defun iquery-forward-replace-regexp ()
  "Interactive Regexp Replace Forwards"
  (interactive)
  (setq lineno (line-number-at-pos (point)))
  (isearch-forward-regexp)
  (goto-line lineno)
  (setq replace (read-from-minibuffer (concat "Replace regexp " (car regexp-search-ring) " with: ")))
  (while (re-search-forward (car regexp-search-ring) nil t)
        (replace-match replace)))

;; Same as above, but for backwards
(defun iquery-backward-replace-regexp ()
  "Interactive Regexp Replace Backwards"
  (interactive)
  (setq lineno (line-number-at-pos (point)))
  (isearch-backward-regexp)
  (goto-line lineno)
  (setq replace (read-from-minibuffer (concat "Replace regexp " (car regexp-search-ring) " with: ")))
  (while (re-search-backward (car regexp-search-ring) nil t)
        (replace-match replace)))

;; I just want bash anyway
(defun bash ()
  "Hey I just want to run bash"
  (interactive)
  (ansi-term "/bin/bash"))

;;http://dse.livejournal.com/67732.html
(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error
     (condition-case nil
	 (fix-frame-horizontal-size width)
       (error
	(error "Cannot resize window or frame horizontally"))))))

;; thanks #eamcs!
(defun apply-function-to-region (beg end fn)
  "Replace the given region with the result of applying the function to it."
    (save-excursion
      (let ((buf-str (buffer-substring-no-properties beg end)))
        (delete-region beg end)
        (insert (funcall fn buf-str)))))

(defun url-encode-region (beg end)
  "url encode the selected region"
  (interactive "r")
  (funcall 'apply-function-to-region beg end 'url-hexify-string))

(defun url-decode-region (beg end)
  "url decode the selected region"
  (interactive "r")
  (funcall 'apply-function-to-region beg end 'url-unhex-string))


;; http://sami.samhuri.net/2007/6/23/emacs-for-textmate-junkies/
(defun surround-region-with-tag (tag-name beg end)
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))

;; http://bc.tech.coop/blog/docs/clojure-emacs.el
(defun check-region-parens ()
  "Check if parentheses in the region are balanced. Signals a
scan-error if not."
  (interactive)
  (save-restriction
    (save-excursion
      (let ((deactivate-mark nil))
        (condition-case c
            (progn
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (/= 0 (- (point)
                              (forward-list))))
              t)
          (scan-error (signal 'scan-error
                              '("Region parentheses not balanced"))))))))


;; http://www.math.umd.edu/~halbert/dotemacs.html
(defun trim-string (str)
  """ Trim whitespace from st."""
  (replace-regexp-in-string "\\(^[ \t\n\r]*\\|[ \t\n\r]*$\\)" "" str))

;; from esk
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))


(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))


(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))


(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; http://blog.tuxicity.se/?p=32
(defun google-region (beg end)
  "Google the selected region."
  (interactive "r")
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" (buffer-substring beg end))))


;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)


;; http://pragmaticemacs.com/emacs/aligning-text/
(defun csb/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun csb/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))


;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun csb/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(defun csb/gentoo-emacs-vcs-p ()
  "Basd on the version string, is this instance probably a vcs
build on gentoo?"
  (and (require 'site-gentoo nil 'noerror)
       ;; The emacs version conention for x.y.z is that y is 0 for pre-release
       (string= (nth 1 (split-string emacs-version "\\."))
                "0")))


(defun esk-pp-json ()
  "Pretty-print the json object following point."
  (interactive)
  (require 'json)
  (let ((json-object (save-excursion (json-read))))
    (switch-to-buffer "*json*")
    (delete-region (point-min) (point-max))
    (insert (pp json-object))
    (goto-char (point-min))))


(provide 'csb/efuncs)
