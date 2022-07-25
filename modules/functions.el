;;; functions.el --- Custom functions for emacs

(defun kc/git-url-to-project-name (url)
  (replace-regexp-in-string "\\(?:https://github.com\/\\|git@github.com:\\)\\(.*\\)\.git$" "\\1"
                            "https://github.com/adi1090x/plymouth-themes.git"))

(defun kc/open-github-file ()
  (interactive)
  (let* ((linenum-str (if (region-active-p)
			  (format "#L%s-L%s"
				  (line-number-at-pos (region-beginning) t)
				  (line-number-at-pos (region-end) t))
			(format "#L%s" (line-number-at-pos nil t))))
	 (filepath (buffer-file-name))
	 (project-root (cdr (project-current)))
	 (filename (file-relative-name filepath project-root))
	 (project-name (kc/git-url-to-project-name
	   		(shell-command-to-string "git remote get-url origin")))
	 (branch (shell-command-to-string "git branch --show-current")))
    (message project-name)
    (browse-url (format "https://github.com/%s/blob/%s/%s%s"
			project-name
	    		branch
			filename
			linenum-str))))

(defun kc/browse-url-mpv (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (message "Opening %s in mpv." url)
  (start-process (format "*mpv* %s" url) nil "mpv" "--cache=yes" url))

(provide 'functions)
;;; functions.el ends here
