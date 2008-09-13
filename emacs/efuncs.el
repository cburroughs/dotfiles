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


