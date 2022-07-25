(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t)

(savehist-mode t)
(recentf-mode t)
(save-place-mode t)
(global-prettify-symbols-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-fringe-mode 0)

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


(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(defun kc/edit-init ()
      (interactive)
      (find-file (locate-user-emacs-file "init.el")))

(global-set-key (kbd "C-ç")
  #'kc/edit-init)

(defun kc/edit-module ()
  (interactive)
  (find-file 
   (completing-read "open file: " (directory-files (locate-user-emacs-file "modules/") t "\\.el\\'") nil t)))

(global-set-key (kbd "C-s-ç") #'kc/edit-module)

;; Auto revert files when they change
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backups/")))
      backup-by-copying t)

(setq use-dialog-box nil)

(provide 'better-defaults)
