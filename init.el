(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(when (not package-archive-contents) ;refresh package list if it's empty
  (package-refresh-contents))

(setq custom-file "~/.emacs.d/custom-file.el") ;separate file for custom.el
(load custom-file 'noerror)

(global-auto-revert-mode t) ;reloads file if changed externally
(setq disabled-command-function nil) ;enables disabled commands
(set-frame-font "Consolas-10") ;why emacs keep resetting my font
(w32-send-sys-command 61488) ;fullscreen

(load "server") ;emacsclient server
(unless (server-running-p) (server-start))

(require 'use-package)

(use-package ample-theme
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(load-theme 'monokai t)

(use-package pacmacs ; M-x pacmacs-start
  :ensure t)

(use-package ido
  :ensure t
  :init
  (ido-mode t))

(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-c m l") 'mc/edit-lines)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this))

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'alien))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-omnisharp))

(use-package csharp-mode
  :ensure t)

(use-package omnisharp
  :ensure t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (setq omnisharp-server-executable-path
        "C:\\Users\\Danniel\\Github\\omnisharp-roslyn\\artifacts\\publish\\OmniSharp\\default\\net46\\omnisharp.exe"))

(use-package tide
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 4)
  (setq web-mode-script-padding 4)
  (setq web-mode-block-padding 4))
