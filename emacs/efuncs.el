; Functions and such

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)
; would like to be able to use shift- arrow to change date


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
         (filename (buffer-file-name))
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


;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))


;; from emacs wiki
;; obviously this would not work without X 
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(global-set-key [f11] 'fullscreen)


; TODO: Investigate these
;http://dotfiles.org/~coder_/.emacs
;; {{{ Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An interactive version of the query-replace-regexp function  that lets you
;; see your regexp search before executing it (Safety first!)
(defun iquery-forward-replace-regexp ()
  "Interactive Regexp Replace Forwards"
  (interactive)
  (setq lineno (line-number-at-pos (point)))
  (isearch-forward-regexp)
  (goto-line lineno)
  (setq replace (read-from-minibuffer (concat "Replace regexp " (first regexp-search-ring) " with: ")))
  (while (re-search-forward (first regexp-search-ring) nil t)
        (replace-match replace)))

;; Same as above, but for backwards
(defun iquery-backward-replace-regexp ()
  "Interactive Regexp Replace Backwards"
  (interactive)
  (setq lineno (line-number-at-pos (point)))
  (isearch-backward-regexp)
  (goto-line lineno)
  (setq replace (read-from-minibuffer (concat "Replace regexp " (first regexp-search-ring) " with: ")))
  (while (re-search-backward (first regexp-search-ring) nil t)
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
