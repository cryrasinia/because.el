(setq-default because/bibliography-path "~/.emacs.d/bibliography.org")
(setq-default because/citation-prefix (concat "file:" because/bibliography-path "#::"))

(defun char-increment
	(c)
  "Increment a character by n."
  (string
   (cond ((and (>= c 65) (<= c 90))
		  (if (= c 90) 65 (+ 1 c)))

		 ((and (>= c 97) (<= c 122))
		  (if (= c 122) 97 (+ 1 c))))))

(defun replace-char-by-index
	(str c idx)
  "Replace the char in a string, at position ${idx} with ${c}."
  (if (or (>= idx (length str)) (> 0 idx))
	  nil ;; Return nil if we're trying to replace a character that doesn't exist in the string
	(concat
	 (substring str 0 idx)
	 c
	 (substring str (+ idx 1) (length str)))))

(defun because/create-citation
	(header id author date_published date_modified title publisher url)
  "Return a citation in Org format."
  (concat "* " header "\n"
		  ":PROPERTIES:\n"
		  ":CUSTOM_ID: " id "\n"
		  ":author: " author "\n"
		  ":date_published: " date_published "\n"
		  ":date_modified: " date_modified "\n"
		  ":title: " title "\n"
		  ":publisher: " publisher "\n"
		  ":url: " url "\n"
		  ":END:"))

(defun because/create-empty-citation
	(&optional header)
  "Return a template for a citation in Org format."
  (because/create-citation header (if (file-exists-p because/bibliography-path) (because/iterate-source-id (because/final-source-id)) "AAA") nil nil nil nil nil nil))

(defun because/append-buffer-to-citation-file
	()
  "Append the current buffer's contents to the citation file."
  (interactive)
  (beginning-of-buffer)
  (insert "\n")
  (append-to-file nil nil because/bibliography-path)
  (kill-this-buffer))

;; Set up citation-mode minor mode
;; Thanks to https://emacs.stackexchange.com/a/520
(setq because/citation-mode-map (make-keymap))
(define-key because/citation-mode-map (kbd "M-s") 'because/append-buffer-to-citation-file)
(define-key because/citation-mode-map (kbd "M-q") 'kill-this-buffer)
(define-minor-mode because/citation-mode
  "Minor mode that adds keys necessary for adding citations."
  :keymap because/citation-mode-map)

(defun because/add-source
	(id author date_published date_modified title publisher url)
  "Add a source to the citations file."
  (because/find-source-list)
  (insert source-template))

(defun because/add-source-interactive
	()
  "Let the user add a source to the citations file interactively."
  (interactive)
  (let ((tmp-buffer
		 (get-buffer-create (generate-new-buffer-name "New Citation"))))
	(switch-to-buffer tmp-buffer)
	(org-mode)
	(because/citation-mode)
	(insert (because/create-empty-citation "New Citation"))))

(defun because/final-source-id
	()
  "Get the final source of the citations file."

  (let ((old-list (buffer-list)))
	(with-temp-buffer
	  (insert-file-contents because/bibliography-path)
	  (end-of-buffer)
	  (org-previous-visible-heading 0)
	  (org-entry-get (point) "CUSTOM_ID"))))

(defun because/cite
	(id)
  "Cite a source from the citations file, in the current buffer."
  (interactive "MSource ID: ")
  (insert (concat "[[" because/citation-prefix (upcase id) "][" (upcase id) "]]")))

(defun because/iterate-source-id
	(id)
  "Given a citation ID format used in the citations file, return an ID iterated it by one."
  (let ((rev-id-list (string-to-list id))
		(i (- (length id) 1))
		(current-char nil)) ;; i = strlen(id) - 1
	(while (not (eq i nil)) ;; Loop through the string backward.
	  (setq current-char (nth i rev-id-list))
	  (if (not (= current-char ?Z)) ;; If our char isn't Z...
		  (progn (setq id (replace-char-by-index id (char-increment current-char) i))  ;; ...just increment, and be done with it.
				 (setq i nil))
		(progn
		  (setq id (replace-char-by-index id "A" i))
		  (setq i (- i 1)))))) ;; If our char was Z, move the index to the next character to the left.
	id)

(provide 'because)
