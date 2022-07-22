(save-place-mode 1)
(global-prettify-symbols-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-fringe-mode 10)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(setq inhibit-startup-message t)

(diminish 'eldoc-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun kc/edit-init ()
      (interactive)
      (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-ç")
  #'kc/edit-init)

(defun kc/edit-module ()
  (interactive)
  (find-file 
   (completing-read "open file: " (directory-files (file-name-concat user-emacs-directory "modules") t "\\.el\\'") nil t)))

(global-set-key (kbd "C-s-ç") #'kc/edit-module)

;; Auto revert files when they change
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backups/")))
      backup-by-copying t)

(provide 'better-defaults)
