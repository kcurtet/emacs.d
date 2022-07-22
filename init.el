;;; init.el --- Emacs Initializationa -*- lexical-binding: t -*-

;;; Commentary:
;; Some comments


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

;;; Modules

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'better-defaults)
(require 'completion.el)
(require 'ui.el)

(set-face-attribute 'default nil :font "Fira Code" :weight 'normal :height 120)


;;; Avy
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2))



;;; Shell
;;;; eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;;; vterm
(use-package vterm)

;;; inbox

(use-package erc
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

(use-package wgrep)

(winner-mode +1)

(use-package pdf-tools
  :init
  (pdf-tools-install))


(use-package dirvish
  :init
  ;; Let Dirvish take over Dired globally
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-size))
  (dirvish-override-dired-mode))

;;; Git
(use-package git-timemachine)

(use-package magit
  :bind ("C-c g" . magit))

;;; Org

(setq org-capture-templates
      '(("r" "Reading List" entry
	 (file+headline "read.org" "To Read")
	 "* TODO %T [ ] %?"
	 :kill-buffer t
	 :empty-lines 1)))

(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-or-filename t)
  (deft-default-extension "org")
  (deft-directory org-directory))

(use-package org-roam
  :bind (("C-c n r f" . org-roam-node-find)
	 ("C-c n r i" . org-roam-node-insert))
  :init
  (setq org-roam-directory (file-truename "~/org/roam"))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam)

;;; Programing

;;;; General
(use-package eglot)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :hook ((js-mode . smartparens-mode)))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-cycle))

;;;; Web
(use-package emmet-mode
  :hook (sgml-mode css-mode)
  :config
  (setq emmet-self-closing-tag-style " /"))

;;;; Common Lisp
(use-package slime)

;;;; Emacs Lisp
(defun kc/emacs-lisp-hook ()
  (outline-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'kc/emacs-lisp-hook)

;;;; Clojure
(use-package cider)

;;; End

(provide 'init)
;;; init.el ends here
