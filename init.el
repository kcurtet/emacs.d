;;; init.el --- Emacs Initializationa -*- lexical-binding: t -*-


;;; straighat.el
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
(setq-default use-package-enable-imenu-support t)

(straight-use-package 'use-package)
(straight-use-package 'diminish)

(use-package no-littering
  :init
  (setq no-littering-etc-directory
	(expand-file-name ".local/" user-emacs-directory))
  (setq no-littering-var-directory
	(expand-file-name ".cache/" user-emacs-directory)))

(use-package gcmh
  :init
  (gcmh-mode 1))

;;; Modules

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'better-defaults)
(require 'completion.el)
(require 'ui.el)

;;; Avy
(use-package avy
  :bind (("M-ç" . avy-goto-word-1)))

;;; Shell
;;;; eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;;; vterm

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://protesilaos.com/codelog.xml"
          "https://nullprogram.com/feed/"
          "http://feeds.feedburner.com/uGeekBlog"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2riBMG3qf1Di20ouRc76BA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w")))

(use-package elfeed-tube
  :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :straight t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))


(use-package writeroom-mode
  :bind ("C-M-z" . writeroom-mode))

(use-package vterm
  :defer t
  :init
  (defun popper-vterm ()
    (interactive)
    (let ((buffer (vterm-other-window)))
      (popper-toggle-type buffer)))
  (global-set-key (kbd "C-x C-'") 'popper-vterm))


;;; inbox

(use-package pinentry
  :defer t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package perspective
  :disabled t
  :bind (("C-x C-b" . persp-list-buffers)       
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         ("C-x M-b" . switch-to-buffer))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(use-package erc
  :commands (erc-tls)
  :init
  (setq erc-server "irc.libera.chat"
	erc-nick "kcurtet"
	erc-user-full-name "Kevin Curtet"
	erc-user-information ""
	erc-prompt-for-password nil
	erc-track-shorten-start 8
	erc-autojoin-channels-alist '((".*\\.libera.chat" . ("#systemcrafters" "#emacs" "#nonguix" "#guix")))
	erc-kill-buffer-on-part t
	erc-auto-query 'bury))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package popper
  :bind (("s-'"   . popper-toggle-latest)
         ("s-?"   . popper-cycle)
         ("C-s-'" . popper-toggle-type)
	 ("M-s-'" . popper-kill-latest-popup))
  :init
  ;; (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
	'("^\\*Warnings\\*"
	  "\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  occur-mode
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; (use-package wgrep
;;   :defer t
;;   :init
;;   (wgrep-setup))

(winner-mode +1)

(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install))


(use-package dirvish
  :defer t
  :disabled t
  :init
  ;; Let Dirvish take over Dired globally
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-size))
  (dirvish-override-dired-mode))

;;; Git
(use-package git-timemachine
  :after magit)

(use-package magit
  :defer t)

;;; Org

(setq org-directory (expand-file-name "~/org"))



(with-eval-after-load 'org

  (org-babel-do-load-languages 'org-babel-load-languages
   '((shell . t)
     (C . t)
     (lisp . t)
     (clojure . t)
     (python . t)
     (js . t)
     (dot . t)
     (emacs-lisp . nil)))

  (setq org-babel-js-cmd "/home/kcurtet/.nvm/versions/node/v16.16.0/bin/node"
	    org-babel-lisp-eval-fn #'sly-eval
	    org-confirm-babel-evaluate nil)
  
  (dolist (value '(("sh" . "src sh")
		           ("py" . "src python")
		           ("js" . "src js")
		           ("clj" . "src clj")))
    (add-to-list 'org-structure-template-alist value))
  
  (setq org-modules '(org-tempo org-habit)
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))
        org-todo-keyword-faces '(("NEXT" . org-warning))
	    org-capture-templates
	    '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("s" "Scratch" entry (file+olp+datetree "~/org/scratch.org")
           "* %T %?\nEntered on %U\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("r" "Reading List" entry (file+headline "to-read.org" "To Read")
	       "* TODO %T [ ] %?"
	       :kill-buffer t
	       :empty-lines 1))))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

;; (use-package org-roam
;;   :bind (("C-c n r f" . org-roam-node-find)
;; 	 ("C-c n r i" . org-roam-node-insert))
;;   :init
;;   (setq org-roam-directory (file-truename "~/org/roam"))
;;   (org-roam-db-autosync-mode))

;; (use-package org-roam-ui
;;   :after org-roam)

(use-package denote
  :bind (("C-c n n" . denote)
         ("C-c n s" . denote-subdirectory)
         ("C-c n i" . denote-link)
         ("C-c n l" . denote-link-find-file)
         ("C-c n b" . denote-link-backlinks)
         ("C-c n o" . (lambda () (interactive) (find-file denote-directory))))
  :init

  (defun denote-blog-entry ()
    "Create Markdown+TOML note in ~/org/blog"
    (interactive)
    (let ((denote-directory (expand-file-name "blog" org-directory))
          (denote-file-type 'markdown-toml))
      (call-interactively #'denote)))
  
  (setq denote-directory (expand-file-name "notes" org-directory))
  (add-hook 'dired-mode-hook #'denote-dired-mode))

;; lookup
(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-or-filename t)
  (deft-default-extension "org")
  (deft-directory denote-directory))

;; Timer 
(use-package tmr)

;;; Programing
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;;; General
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(use-package docker-compose-mode
  :mode "\\docker-compose.ya?ml\\'")

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :hook ((js-mode . smartparens-mode)))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (cider-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-cycle))

;;;; Web
(use-package emmet-mode
  :hook (sgml-mode css-mode)
  :config
  (setq emmet-self-closing-tag-style " /"))

;;;; Haskell
(use-package haskell-mode)

;;;; Common Lispe

(use-package sly
  :init
  (setq inferior-lisp-program "/home/kcurtet/.asdf/shims/sbcl")
  (sly-setup))

;;;; Emacs Lisp
(defun kc/emacs-lisp-hook ()
  (outline-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'kc/emacs-lisp-hook)

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

(define-key lisp-interaction-mode-map (kbd "C-M-ñ") #'eval-print-last-sexp)
;;;; Clojure
(use-package cider
  :config
  (setq cider-repl-result-prefix ";; => "))

;;;; Java
(use-package javadoc-lookup
  :bind ("C-h j" . javadoc-lookup)
  :config
  (javadoc-add-roots "/usr/share/doc/openjdk-8-jdk/api/"
                     "/usr/share/doc/openjdk-11-jdk/api/"
                     "/usr/share/doc/openjdk-17-jdk/api/"))

;;;; Python
(use-package anaconda-mode
  :hook (python-mode))

;;; End

(provide 'init)
;;; init.el ends here
