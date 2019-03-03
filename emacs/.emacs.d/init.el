;;; Package Management

;; added by package.el
;; (package-initialize)

(require 'package)

(setq-default package-archives
              '(("gnu" . "https://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
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

(use-package quelpa)
(use-package quelpa-use-package)
(setq quelpa-update-melpa-p nil)
(quelpa-use-package-activate-advice)

;;; Extra Files
(use-package no-littering)

(defalias 'panda-var-file 'no-littering-expand-var-file-name)
(defalias 'panda-etc-file 'no-littering-expand-etc-file-name)

(setq custom-file (panda-etc-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Libraries
(require 'cl)
(use-package dash)
(use-package s)
(use-package hydra)

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

(use-package display-line-numbers
  :bind (("C-c l" . panda-toggle-line-number))
  :init
  (setq-default display-line-numbers-type 'relative)
  :config
  (defun panda-display-line-numbers-in-frame (frame)
    "Display line numbers in `frame'."
    (with-selected-frame frame
      (global-display-line-numbers-mode 1)))
  (add-to-list 'after-make-frame-functions #'panda-display-line-numbers-in-frame)
  (defun panda-toggle-line-numbers ()
    "Toggle between relative and absolute line numbers in current buffer."
    (interactive)
    (setq-local display-line-numbers-type (case display-line-numbers-type
                                            (relative t)
                                            ((t) 'relative)
                                            (otherwise 'relative)))
    (display-line-numbers-mode 1))
  (global-display-line-numbers-mode 1)
  (column-number-mode 1))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon nil)
  :config
  (doom-modeline-init))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Basic Configuration
;;;; Defaults
(setq auto-save-default nil
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup"))
      delete-by-moving-to-trash t
      disabled-command-function nil
      inhibit-compacting-font-caches t
      make-backup-files nil
      save-abbrevs nil
      vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)

(global-auto-revert-mode t)

;;;; Definitions
;;;;; Constants
(defconst panda-neon-green "#39FF14")
(defconst panda-light-blue "#67C8FF")
(defconst panda-deep-saffron "#FF9933")

;;;;; Modifications
(defun panda-end-isearch-forward ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(add-to-list 'isearch-mode-end-hook #'panda-end-isearch-forward)

;;;;; Help
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;;; Global Packages
;;;; Multi-Purpose
(use-package flx)
(use-package smex)

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("<return>" . ivy-alt-done))
  :init
  (setq ivy-wrap t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        confirm-nonexistent-file-or-buffer t
        ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :foreground panda-neon-green)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :foreground panda-light-blue)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :foreground panda-deep-saffron)
  (set-face-attribute 'ivy-confirm-face nil
                      :foreground panda-neon-green))

(use-package counsel
  :bind (("C-c r" . counsel-rg))
  :config
  (counsel-mode 1))

;;;; Executing Code
(use-package quickrun
  :bind (("C-c q" . quickrun)
         ("C-c Q" . quickrun-shell)))

(use-package realgud)

;;;; Editing
(use-package expand-region
  :bind (("C-'" . expand-region)))

(use-package undo-tree
  :init
  (setq undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode))

;;;; Git
(use-package magit
  :bind (("C-c g" . magit-status))
  :init
  (setq magit-auto-revert-mode nil))

(use-package git-timemachine
  :bind (("C-c t" . git-timemachine)))

;;;; Navigation
(use-package avy
  :bind (("C-;" . avy-goto-char-timer))
  :init
  (setq avy-background t)
  :config
  (set-face-attribute 'avy-lead-face nil
                      :foreground panda-neon-green
                      :background (face-attribute 'default :background))
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground panda-light-blue
                      :background (face-attribute 'default :background))
  (set-face-attribute 'avy-lead-face-2 nil
                      :foreground panda-deep-saffron
                      :background (face-attribute 'default :background)))

(use-package dired-sidebar
  :bind (("C-c d" . dired-sidebar-toggle-sidebar))
  :init
  (setq dired-sidebar-theme 'none))

(use-package imenu
  :bind (("C-c i" . imenu))
  :init
  (setq imenu-auto-rescan t))

(use-package projectile
  :bind (("C-c p" . projectile-command-map))
  :init
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;;; Windows
(use-package eyebrowse
  :init
  (defvar eyebrowse-mode-map (make-sparse-keymap))
  :config
  (eyebrowse-mode 1))

(use-package winner
  :config
  (winner-mode 1))

(defhydra panda-manage-windows (:hint nil :color blue)
  "
 Window Configs^^^^                    Manage Window Configs^^    Undo/Redo Window Changes^^
---------------^^^^-----------------------------------------^^----------------------------^^-
 [_1_]: config 1    [_6_]: config 6    [_s_]: switch config       [_u_]: undo
 [_2_]: config 2    [_7_]: config 7    [_c_]: create config       [_r_]: redo
 [_3_]: config 3    [_8_]: config 8    [_t_]: tag config
 [_4_]: config 4    [_9_]: config 9    [_k_]: kill config
 [_5_]: config 5    [_0_]: config 0"
  ;; Switch Window Configs
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("0" eyebrowse-switch-to-window-config-0)
  ;; Manage Window Configs
  ("s" eyebrowse-switch-to-window-config)
  ("c" eyebrowse-create-window-config)
  ("t" eyebrowse-rename-window-config)
  ("k" eyebrowse-close-window-config)
  ;; Undo/Redo Window Changes
  ("u" winner-undo :color red)
  ("r" winner-redo :color red)
  ;; Quit
  ("q" nil :exit t))

(bind-key "C-c w" #'panda-manage-windows/body)

;;; Per-Language Configuration
;;;; Completion / Linting
(use-package company
  :init
  (setq company-dabbrev-code-modes nil
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  :config
  (delete 'company-dabbrev company-backends))

(use-package flymake
  :bind (("M-p" . flymake-goto-prev-error)
         ("M-n" . flymake-goto-next-error)))

;;;; Formatting
(defun panda-generic-format-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(defun panda-generic-format-on-save ()
  (add-hook 'before-save-hook #'panda-generic-format-buffer nil t))

(defun panda-trim-whitespace-on-save ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(defun panda-default-clang-format-style ()
  "Obtain the default clang-format style as a string."
  (with-temp-buffer
    (insert-file-contents (panda-etc-file "clang-format-defaults.json"))
    (let ((inhibit-message t))
      (replace-regexp "[\n\"]" ""))
    (buffer-string)))

(use-package reformatter
  :config
  (cl-defmacro panda-reformatter-define (name &rest key-pairs)
    "A wrapper around `reformatter-define' that also creates
a variable for the formatter program's arguments."
    (declare (indent defun))
    (assert (symbolp name))
    (let* ((args-symbol (intern (format "%s-args" name)))
           (args (plist-get key-pairs :args)))
      (plist-put key-pairs :args args-symbol)
      `(progn
         (defvar ,args-symbol ,args
           "Arguments for the formatter program.")
         (reformatter-define ,name
           ,@key-pairs))))
  (panda-reformatter-define asmfmt
    :program "asmfmt")
  (panda-reformatter-define black
    :program "black"
    :args '("-" "--quiet" "--line-length" "80"))
  (panda-reformatter-define brittany
    :program "brittany")
  (panda-reformatter-define clang-format
    :program "clang-format"
    :args (list "-style" (panda-default-clang-format-style)))
  (panda-reformatter-define dfmt
    :program "dfmt"
    :args '("--brace_style=otbs" "--space_after_cast=false" "--max_line_length=80"))
  (panda-reformatter-define gofmt
    :program "gofmt")
  (panda-reformatter-define prettier-html
    :program "prettier"
    :args '("--stdin" "--parser" "html"))
  (panda-reformatter-define prettier-css
    :program "prettier"
    :args '("--stdin" "--parser" "css" "--tab-width" "4"))
  (panda-reformatter-define prettier-javascript
    :program "prettier"
    :args '("--stdin" "--parser" "javascript" "--tab-width" "4"))
  (panda-reformatter-define prettier-markdown
    :program "prettier"
    :args '("--stdin" "--parser" "markdown"))
  (panda-reformatter-define prettier-typescript
    :program "prettier"
    :args '("--stdin" "--parser" "typescript" "--tab-width" "4"))
  (panda-reformatter-define rustfmt
    :program "rustfmt"))

;;;; Language Server
(use-package eglot)

;;;; Lisp
(use-package lispy
  :config
  (lispy-set-key-theme '(lispy special)))

;;;; Organization
(use-package outshine)

;;;; Snippets
(use-package yasnippet
  :init
  (setq yas-triggers-in-field nil
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :bind (("C-c y" . ivy-yasnippet)))

;;; Language Modes
;;;; Assembly
(defun panda-setup-asm-mode ()
  (asmfmt-on-save-mode 1)
  (yas-minor-mode 1)
  (setq indent-tabs-mode t)
  (setq-local tab-always-indent (default-value 'tab-always-indent)))

(use-package asm-mode
  :init
  (setq asm-comment-char ?#)
  :config
  (add-hook 'asm-mode-hook #'panda-setup-asm-mode))

;;;; C / C++
(defun panda-setup-c-mode ()
  (company-mode 1)
  (clang-format-on-save-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1)
  (c-set-offset 'innamespace 0))

(add-hook 'c-mode-hook #'panda-setup-c-mode)
(add-hook 'c++-mode-hook #'panda-setup-c-mode)

;;;; CMake
(defun panda-setup-cmake-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package cmake-mode
  :config
  (add-hook 'cmake-mode-hook #'panda-setup-cmake-mode))

;;;; Common Lisp
(defalias 'panda-setup-common-lisp-mode 'panda-setup-emacs-lisp-mode)

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (add-hook 'slime-mode-hook #'panda-setup-common-lisp-mode)
  (slime-setup '(slime-fancy)))

;;;; D
(defun panda-setup-d-mode ()
  (company-mode 1)
  (dfmt-on-save-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1))

(use-package d-mode
  :config
  (add-to-list 'eglot-server-programs '(d-mode . ("dls")))
  (add-hook 'd-mode-hook #'panda-setup-d-mode))

;;;; Emacs Lisp
(defun panda-setup-emacs-lisp-mode ()
  (company-mode 1)
  (lispy-mode 1)
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'panda-setup-emacs-lisp-mode)

;;;; Git Files
(defun panda-setup-gitfiles-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

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
  (eglot-ensure)
  (gofmt-on-save-mode 1)
  (yas-minor-mode 1)
  (setq indent-tabs-mode t))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'panda-setup-go-mode))

;;;; Haskell
(defun panda-setup-haskell-mode ()
  (brittany-on-save-mode 1)
  (company-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'panda-setup-haskell-mode))

;;;; HTML
(defun panda-setup-web-mode ()
  (prettier-html-on-save-mode 1)
  (yas-minor-mode 1))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4)
  :config
  (add-hook 'web-mode-hook #'panda-setup-web-mode))

;;;; Java
(defun panda-setup-java-mode ()
  (clang-format-on-save-mode 1)
  (yas-minor-mode 1))

(add-hook 'java-mode-hook #'panda-setup-java-mode)

;;;; JavaScript
(defun panda-setup-javascript-mode ()
  (company-mode 1)
  (eglot-ensure)
  (prettier-javascript-on-save-mode 1)
  (yas-minor-mode 1))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'panda-setup-javascript-mode))

;;;; Latex
(defun panda-setup-latex-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'LaTeX-mode-hook #'panda-setup-latex-mode)

(use-package tex
  :ensure auctex
  :init
  (setq TeX-auto-save t
        TeX-parse-self t))

;;;; Makefile
(defun panda-setup-makefile-mode ()
  (panda-trim-whitespace-on-save)
  (yas-minor-mode 1))

(add-hook 'makefile-mode-hook #'panda-setup-makefile-mode)

;;;; Markdown
(defun panda-setup-markdown-mode ()
  (prettier-markdown-on-save-mode 1)
  (yas-minor-mode 1))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'panda-setup-markdown-mode))

;;;; Org
(defun panda-setup-org-mode ()
  (panda-generic-format-on-save))

(use-package org
  :init
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  :config
  (add-hook 'org-mode-hook #'panda-setup-org-mode))

;;;; Python
(defun panda-setup-python-mode ()
  (black-on-save-mode 1)
  (company-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1)
  (setq-local yas-indent-line 'fixed)
  (setq-local yas-also-auto-indent-first-line nil))

(use-package python
  :init
  (setq python-indent-offset 4)
  :config
  (add-hook 'python-mode-hook #'panda-setup-python-mode))

;;;; R
(defun panda-setup-r-mode ()
  (company-mode 1)
  (eglot-ensure)
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package ess
  :init
  (setq ess-ask-for-ess-directory nil
        ess-use-flymake nil)
  :config
  (add-hook 'ess-r-mode-hook #'panda-setup-r-mode))

;;;; Rust
(defun panda-setup-rust-mode ()
  (company-mode 1)
  (eglot-ensure)
  (rustfmt-on-save-mode 1)
  (yas-minor-mode 1))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'panda-setup-rust-mode))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;;;; TypeScript
(defun panda-setup-typescript-mode ()
  (company-mode 1)
  (eglot-ensure)
  (prettier-typescript-on-save-mode 1)
  (yas-minor-mode 1))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'panda-setup-typescript-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; End:
