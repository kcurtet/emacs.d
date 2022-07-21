(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(straight-use-package 'diminish)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(save-place-mode)
(global-prettify-symbols-mode)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq inhibit-startup-message t)

(diminish 'eldoc-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'completion.el)

(use-package paredit
  :diminish paredit-mode
  :hook (prog-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(global-set-key (kbd "C-c C-p")
  #'(lambda ()
      (interactive)
      (find-file (expand-file-name "init.el" user-emacs-directory))))

(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2))


(defun open-github-file (project branch file)
  (interactive)
  (let* ((filepath (buffer-file-name))
	 (path (cdr (project-current)))))
  (browse-url (format "https://github.com/%s/blob/%s/%s"
		      project
		      branch
		      file)))
(use-package magit
  :bind ("C-c g" . magit))
