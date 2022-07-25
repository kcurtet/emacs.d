(use-package all-the-icons)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120 :width 'normal)
(set-face-attribute 'variable-pitch nil :font "CodeNewRoman Nerd Font" :height 130)

(use-package treemacs
  :defer t)

(use-package modus-themes)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :after doom-themes
  :hook (after-init . doom-modeline-mode))

(add-to-list 'default-frame-alist '(alpha 90 90))

(provide 'ui.el)
