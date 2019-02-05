;;; Package Management

;; added by package.el
;; (package-initialize)

(require 'package)

(setq-default package-archives
              '(("gnu"     . "https://elpa.gnu.org/packages/")
                ("melpa"        . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/"))
              package-archive-priorities
              '(("gnu" . 1)
                ("melpa" . 10)
                ("melpa-stable" . 0)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-demand t)

;;; Customize File
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Evil
(use-package goto-chg)

(use-package evil
  :custom
  (evil-move-beyond-eol nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-want-keybinding nil)
  (evil-want-Y-yank-to-eol t)
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "fd")
  (evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

(use-package evil-anzu
  :after evil)

;;; Leader Keymap
(use-package general
  :config
  (general-override-mode)
  (general-evil-setup)
  (general-define-key
   :states '(insert normal operator motion visual)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-p"
   :prefix-map 'panda-leader-map)
  (general-create-definer panda-general-leader
    :keymaps 'panda-leader-map))

;;; Appearance
(setq default-frame-alist '((fullscreen . maximized)
                            (font . "Consolas-11")
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil))
      inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil)

(use-package monokai-theme)
(load-theme 'monokai t)

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (doom-modeline-init))

(use-package display-line-numbers
  ;; built into emacs 26
  :general
  (panda-general-leader "l" 'panda-toggle-line-numbers)
  :custom
  (display-line-numbers-type 'relative)
  :config
  (defun panda-toggle-line-numbers ()
    (interactive)
    (setq display-line-numbers-type (if (eq display-line-numbers-type t)
                                        'relative
                                      t))
    (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode 1))
  (global-display-line-numbers-mode 1)
  (column-number-mode 1))

(use-package beacon
  :diminish beacon-mode
  :custom
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves nil)
  :config
  (beacon-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Basic Configuration
;;;; Defaults
(setq auto-save-default nil
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux"))
      disabled-command-function nil
      inhibit-compacting-font-caches t
      make-backup-files nil
      vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4)

(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)

(global-auto-revert-mode t)

;;;; Key Definitions
;;;;; Remaps
(panda-general-leader
  "k" 'kill-buffer
  "o" 'occur
  "O" 'multi-occur)

;;;;; Keybind Help
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;;;; Constants
(defconst panda-neon-green "#39FF14")
(defconst panda-light-blue "#67C8FF")
(defconst panda-deep-saffron "#FF9933")

;;; Miscellaneous Packages
(use-package esup)
(use-package hydra)

;;; Global Packages
;;;; Multi-Purpose
(use-package flx)
(use-package smex)

(use-package ivy
  :diminish ivy-mode
  :general
  (general-def
    :keymaps 'ivy-minibuffer-map
    "<return>" 'ivy-alt-done)
  :custom
  (ivy-wrap t)
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  (confirm-nonexistent-file-or-buffer t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :foreground panda-neon-green
                      :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :foreground panda-light-blue
                      :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :foreground panda-deep-saffron
                      :weight 'bold)
  (set-face-attribute 'ivy-confirm-face nil
                      :foreground panda-neon-green))

(use-package counsel
  :general
  (panda-general-leader
    "f" 'counsel-find-file
    "r" 'counsel-rg)
  :config
  (counsel-mode 1))

;;;; Executing Code
(use-package quickrun)
(use-package realgud)

;;;; Editing
(use-package evil-mc
  :general
  (panda-general-leader "m" 'panda-evil-mc/body)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (defhydra panda-evil-mc (:hint nil :color pink :post (anzu--reset-mode-line))
    "
  evil-mc
  [_c_]: make cursor here     [_a_]: make cursors (all)    [_s_]: stop cursors          [_r_]: resume cursors
  [_p_]: prev match           [_n_]: next match            [_b_]: prev cursor           [_f_]: next cursor
  [_P_]: prev match (skip)    [_N_]: next match (skip)     [_B_]: prev cursor (skip)    [_F_]: next cursor (skip)
  [_u_]: undo all             [_/_]: cancel"
    ("c" evil-mc-make-cursor-here)
    ("a" evil-mc-make-all-cursors)
    ("s" evil-mc-pause-cursors)
    ("r" evil-mc-resume-cursors)
    ("p" evil-mc-make-and-goto-prev-match)
    ("n" evil-mc-make-and-goto-next-match)
    ("b" evil-mc-make-and-goto-prev-cursor)
    ("f" evil-mc-make-and-goto-next-cursor)
    ("P" evil-mc-skip-and-goto-prev-match)
    ("N" evil-mc-skip-and-goto-next-match)
    ("B" evil-mc-skip-and-goto-prev-cursor)
    ("F" evil-mc-skip-and-goto-next-cursor)
    ("u" evil-mc-undo-all-cursors :color blue)
    ("/" (message "Abort") :color blue))
  (global-evil-mc-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :general
  (panda-general-leader "u" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode))

;;;; Git
(use-package magit
  :general
  (panda-general-leader "g" 'magit-status)
  :custom
  (magit-auto-revert-mode nil))

(use-package evil-magit
  :after magit)

(use-package git-timemachine
  :general
  (panda-general-leader "t" 'git-timemachine))

;;;; Navigation
(use-package avy
  :general
  (panda-general-leader "SPC" 'avy-goto-word-1)
  :custom
  (avy-background t)
  :config
  (set-face-attribute 'avy-lead-face nil
                      :foreground panda-neon-green
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground panda-light-blue
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'avy-lead-face-2 nil
                      :foreground panda-deep-saffron
                      :background (face-attribute 'default :background)
                      :weight 'bold))

(use-package imenu
  :general
  (panda-general-leader "i" 'imenu)
  :custom
  (imenu-auto-rescan t))

(use-package projectile
  :general
  (panda-general-leader
    :prefix "p"
    :prefix-command 'projectile-command-map)
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;;; Windows
(use-package eyebrowse
  :general
  (panda-general-leader
    "0" 'eyebrowse-switch-to-window-config-0
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9)
  :config
  (eyebrowse-mode 1))

;;; Per-Language Packages
(use-package company
  :general
  (general-def :keymaps 'company-active-map
    "<return>" 'company-complete-selection)
  :custom
  (company-dabbrev-code-modes nil)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :config
  (delete 'company-dabbrev company-backends))

(use-package format-all)

(use-package flycheck
  :general
  (panda-general-leader "e" 'panda-flycheck/body)
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (defhydra panda-flycheck (:hint nil :color pink)
    "
  flycheck
  [_p_]: previous error    [_n_]: next error    [_/_]: cancel"
    ("p" flycheck-previous-error)
    ("n" flycheck-next-error)
    ("/" (message "Abort") :color blue)))

(use-package lispy)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode))

(use-package lsp-mode
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-prefer-flymake nil)
  :config
  (require 'lsp-clients))

(use-package company-lsp
  :after lsp-mode)

(use-package lsp-ui
  :after lsp-mode)

(use-package reformatter)

(use-package outshine)

(use-package yasnippet
  :general
  (general-def :keymaps 'yas-minor-mode-map
    "<tab>" nil
    "TAB" nil
    "<backtab>" 'yas-expand)
  :custom
  (yas-triggers-in-field nil)
  (yas-indent-line 'auto)
  (yas-also-auto-indent-first-line t)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-reload-all)
  (eval-after-load 'company
    (define-advice company-select-previous (:around (old-func &rest args))
      (unless (and (bound-and-true-p yas-minor-mode) (yas-expand))
        (call-interactively old-func args)))))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package ivy-yasnippet
  :after yasnippet
  :general
  (panda-general-leader "y" 'ivy-yasnippet))

;;; Language Modes
;;;; Assembly
(defun panda-setup-asm-mode ()
  (format-all-mode 1)
  (yas-minor-mode 1)
  (setq indent-tabs-mode t)
  (setq-local tab-always-indent (default-value 'tab-always-indent)))

(use-package asm-mode
  :custom
  (asm-comment-char ?#)
  :config
  (add-hook 'asm-mode-hook #'panda-setup-asm-mode))

;;;; C / C++
(defun panda-setup-c-mode ()
  (yas-minor-mode 1)
  (c-set-style "linux")
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'panda-setup-c-mode)
(add-hook 'c++-mode-hook #'panda-setup-c-mode)

(use-package ccls
  :hook ((c-mode c++-mode) . lsp))

(use-package clang-format
  :hook ((c-mode c++-mode) . panda-enable-clang-format)
  :config
  (defvar panda-clang-format-settings-file
    (expand-file-name "clang-format-defaults.json" user-emacs-directory)
    "A JSON file containing default clang-format settings.")
  (defun panda-default-clang-format-style ()
    "Reads the JSON file defined by `panda-clang-format-settings-file'"
    (with-temp-buffer
      (insert-file-contents panda-clang-format-settings-file)
      (let ((inhibit-message t))
        (replace-regexp "[\n\"]" ""))
      (buffer-string)))
  (defun panda-enable-clang-format ()
    (setq-local clang-format-style
                (if (locate-dominating-file "." ".clang-format")
                    "file"
                  (panda-default-clang-format-style)))
    (add-hook 'before-save-hook #'clang-format-buffer nil t)))

;;;; C#
(defun panda-setup-csharp-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook #'panda-setup-csharp-mode))

(use-package omnisharp
  :init
  (add-hook 'csharp-mode-hook #'omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp))

;;;; CMake
(defun panda-setup-cmake-mode ()
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package cmake-mode
  :config
  (add-hook 'cmake-mode-hook #'panda-setup-cmake-mode))

;;;; Clojure
(defun panda-setup-clojure-mode ()
  (lispy-mode 1)
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'panda-setup-clojure-mode))

(use-package cider
  :config
  (add-hook 'cider-mode-hook (lambda ()
                               (interactive)
                               (company-mode 1)
                               (add-hook 'before-save-hook #'cider-format-buffer nil t))))

;;;; Common Lisp
(defun panda-setup-slime-mode ()
  (lispy-mode 1)
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package slime
  :config
  (add-hook 'slime-mode-hook #'panda-setup-slime-mode)
  (setq inferior-lisp-program (executable-find "sbcl"))
  (slime-setup '(slime-fancy)))

;;;; D
(reformatter-define panda-dfmt
  :program "dfmt"
  :args '("--brace_style=otbs" "--space_after_cast=false" "--max_line_length=80"))

(defun panda-setup-d-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (panda-dfmt-on-save-mode 1)
  (yas-minor-mode 1))

(use-package d-mode
  :config
  (add-hook 'd-mode-hook #'panda-setup-d-mode))

(use-package company-dcd
  :hook (d-mode . company-dcd-mode))

(use-package flycheck-dmd-dub
  :hook (d-mode . flycheck-dmd-dub-set-variables))

;;;; Emacs Lisp
(defun panda-setup-emacs-lisp-mode ()
  (company-mode 1)
  (format-all-mode 1)
  (lispy-mode 1)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'panda-setup-emacs-lisp-mode)

;;;; Git Files
(defun panda-setup-gitfiles-mode ()
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package gitattributes-mode
  :config
  (add-hook 'gitattributes-mode-hook #'panda-setup-gitfiles-mode))

(use-package gitconfig-mode
  :config
  (add-hook 'gitconfig-mode-hook #'panda-setup-gitfiles-mode))

(use-package gitignore-mode
  :config
  (add-hook 'gitignore-mode-hook #'panda-setup-gitfiles-mode))

;;;; Go
(defun panda-setup-go-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (format-all-mode 1)
  (yas-minor-mode 1)
  (setq indent-tabs-mode t))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'panda-setup-go-mode))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go))

;;;; Haskell
(defun panda-setup-haskell-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (format-all-mode 1)
  (yas-minor-mode 1))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'panda-setup-haskell-mode))

(use-package intero
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (flycheck-add-next-checker 'intero '(info . haskell-hlint)))

;;;; HTML / PHP / ASP.NET / Embedded Ruby
(defun panda-setup-web-mode ()
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package web-mode
  :mode (("\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook #'panda-setup-web-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4))

;;;; Java
(defun panda-setup-java-mode ()
  (yas-minor-mode 1)
  (panda-enable-clang-format))

(add-hook 'java-mode-hook #'panda-setup-java-mode)

;;;; JavaScript
(defun panda-setup-javascript-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (panda-enable-clang-format))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'panda-setup-javascript-mode))

(use-package tern
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

;;;; Latex
(defun panda-setup-latex-mode ()
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'LaTeX-mode-hook #'panda-setup-latex-mode)

(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

;;;; Makefile
(defun panda-setup-makefile-mode ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'makefile-mode-hook #'panda-setup-makefile-mode)

;;;; Markdown
(defun panda-setup-markdown-mode ()
  (format-all-mode 1)
  (yas-minor-mode 1))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'panda-setup-markdown-mode))

;;;; Org
(defun panda-setup-org-mode ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package org
  :config
  (add-hook 'org-mode-hook #'panda-setup-org-mode)
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t))

(use-package evil-org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

;;;; PowerShell
(defun panda-setup-powershell-mode ()
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package powershell
  :config
  (add-hook 'powershell-mode-hook #'panda-setup-powershell-mode))

;;;; Python
(defun panda-setup-python-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (setq-local yas-indent-line 'fixed)
  (setq-local yas-also-auto-indent-first-line nil))

(use-package python
  :config
  (add-hook 'python-mode-hook #'panda-setup-python-mode)
  (setq python-indent-offset 4))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-line-length 80))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

;;;; R
(reformatter-define panda-styler
  ;; styler slow af
  :program (expand-file-name "styler.R" user-emacs-directory))

(defun panda-setup-r-mode ()
  (company-mode 1)
  (panda-styler-on-save-mode 1)
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package ess
  :commands R
  :config
  (add-hook 'ess-r-mode-hook #'panda-setup-r-mode))

;;;; Rust
(defun panda-setup-rust-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'panda-setup-rust-mode)
  (setq rust-format-on-save t))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer
  :init
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;;;; TypeScript
(defun panda-setup-typescript-mode ()
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'panda-setup-typescript-mode))

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (add-hook 'before-save-hook #'tide-format-before-save nil t))
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; End:
