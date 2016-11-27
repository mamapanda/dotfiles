
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(when (not package-archive-contents) ;refresh package list if it's empty
  (package-refresh-contents))

(setq custom-file "~/.emacs.d/custom-file.el") ;separate file for custom.el
(load custom-file 'noerror)

(w32-send-sys-command 61488) ;fullscreen

(load "server")
(unless (server-running-p) (server-start))

(require 'use-package)

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))

(use-package ido
  :ensure t
  :init
  (ido-mode t))

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
  (setq company-idle-delay 0.3))

(use-package tide
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :config
  (setq company-tooltip-align-annotations t))
