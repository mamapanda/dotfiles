(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(unless package-archive-contents ;refresh package list if it's empty
  (package-refresh-contents))

(setq custom-file "~/.emacs.d/custom-file.el") ;separate file for custom.el
(load custom-file 'noerror)

(global-auto-revert-mode t) ;reloads file if changed externally
(setq disabled-command-function nil) ;enables disabled commands
(set-frame-font "Consolas-10") ;why emacs keep resetting my font
(w32-send-sys-command 61488) ;fullscreen

(load "server") ;emacsclient server
(unless (server-running-p)
  (server-start))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package hydra)

(use-package ample-theme)
(use-package atom-one-dark-theme)
(use-package monokai-theme)
(use-package solarized-theme)
(use-package spacemacs-theme)

(load-theme 'monokai t)

(use-package fireplace)
(use-package pacmacs)

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode t))

(use-package avy
  :bind (("C-c a" . avy-goto-word-1)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

(use-package expand-region
  :bind (("C-;" . er/expand-region)))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)))

(use-package ido
  :init
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1)
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-ubiquitous
  :init
  (ido-ubiquitous-mode t))

(use-package multiple-cursors
  :init
  (defhydra hydra-multiple-cursors (global-map "C-c m")
    ("p" mc/mark-previous-like-this)
    ("n" mc/mark-next-like-this)
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("q" nil)))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'alien))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :bind (("M-x" . smex))
  :init
  (smex-initialize))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-." . undo-tree-redo)
              ("C-?" . nil))
  :init
  (global-undo-tree-mode))

(use-package winner
  :init
  (winner-mode t))

(use-package yasnippet
  :init
  (yas-global-mode t)
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (yas-reload-all)
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'auto)
  (setq yas-also-auto-indent-first-line t)
  (defun company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-fallback-behavior nil))
      (unless (yas-expand)
        (call-interactively #'company-complete-common))))
  (defun company-yas-tab ()
    (substitute-key-definition 'company-complete-common
                               'company-yasnippet-or-completion
                               company-active-map))
  (add-hook 'company-mode-hook #'company-yas-tab))

(use-package irony
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  (defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

(use-package company-irony
  :after company
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package company-irony-c-headers
  :after company
  :init
  (delete 'company-irony company-backends)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(use-package csharp-mode)

(use-package omnisharp
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (setq omnisharp-server-executable-path
        "~/.emacs.d/omnisharp-roslyn/artifacts/publish/OmniSharp/default/net46/omnisharp.exe")
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp)))

(use-package clojure-mode)

;(use-package cider) ;lag

(use-package haskell-mode)

(use-package typescript-mode)

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 4)
  (setq web-mode-script-padding 4)
  (setq web-mode-block-padding 4))
