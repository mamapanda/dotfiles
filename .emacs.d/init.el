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
  (evil-want-fine-undo nil)
  (evil-want-keybinding nil)
  (evil-want-Y-yank-to-eol t)
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (evil-global-set-key 'insert (kbd "C-z") nil)
  (evil-global-set-key 'motion (kbd "C-z") nil)
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
  (general-create-definer panda-override-evil
    :states '(normal operator motion visual)
    :keymaps 'override)
  (panda-override-evil
    :prefix "<backspace>"
    :prefix-map 'panda-leader-map)
  (general-create-definer panda-leader-def
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

(column-number-mode 1)

(use-package monokai-theme)
(load-theme 'monokai t)

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (doom-modeline-init))

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
      delete-by-moving-to-trash t
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
  (panda-leader-def
    "f" 'counsel-find-file
    "r" 'counsel-rg)
  :config
  (counsel-mode 1))

;;;; Executing Code
(use-package quickrun)
(use-package realgud)

;;;; Editing
(use-package auto-yasnippet
  :general
  (panda-leader-def
    "a" 'aya-expand
    "A" 'aya-create))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-mc
  :general
  (panda-leader-def "m" 'panda-evil-mc/body)
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

(use-package expand-region
  :general
  (general-vmap "v" 'er/expand-region))

(use-package undo-tree
  :general
  (panda-leader-def "u" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode))

;;;; Git
(use-package magit
  :general
  (panda-leader-def "g" 'magit-status)
  :custom
  (magit-auto-revert-mode nil))

(use-package evil-magit
  :after magit)

(use-package git-timemachine
  :general
  (panda-leader-def "t" 'git-timemachine))

;;;; Navigation
(use-package avy
  :general
  (panda-override-evil
    "SPC" 'avy-goto-word-1
    "S-SPC" 'avy-goto-line)
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

(use-package evil-snipe
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-scope 'visible)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package imenu
  :general
  (panda-leader-def "i" 'imenu)
  :custom
  (imenu-auto-rescan t))

(use-package projectile
  :general
  (panda-leader-def
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
  (panda-leader-def
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

;;; Per-Language Configuration
;;;; Completion / Linting
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

(use-package flymake
  :general
  (general-def :keymaps 'flymake-mode-map
    "M-p" 'flymake-goto-prev-error
    "M-n" 'flymake-goto-next-error))

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

(use-package reformatter
  :config
  (reformatter-define asmfmt
    :program "asmfmt")
  (reformatter-define black
    :program "black"
    :args '("-" "--quiet" "--line-length" "80"))
  (reformatter-define brittany
    :program "brittany")
  (reformatter-define clang-format
    :program "clang-format"
    :args `("-style" ,(if (locate-dominating-file "." ".clang-format")
                          "file"
                        (panda-default-clang-format-style))))
  (reformatter-define dfmt
    :program "dfmt"
    :args '("--brace_style=otbs" "--space_after_cast=false" "--max_line_length=80"))
  (reformatter-define gofmt
    :program "gofmt")
  (reformatter-define prettier-html
    :program "prettier"
    :args '("--stdin" "--parser" "html"))
  (reformatter-define prettier-css
    :program "prettier"
    :args '("--stdin" "--parser" "css" "--tab-width" "4"))
  (reformatter-define prettier-javascript
    :program "prettier"
    :args '("--stdin" "--parser" "javascript" "--tab-width" "4"))
  (reformatter-define prettier-markdown
    :program "prettier"
    :args '("--stdin" "--parser" "markdown"))
  (reformatter-define prettier-typescript
    :program "prettier"
    :args '("--stdin" "--parser" "typescript" "--tab-width" "4"))
  (reformatter-define rustfmt
    :program "rustfmt")
  (reformatter-define styler
    :program (expand-file-name "styler.R" user-emacs-directory)))

;;;; Language Server
(use-package eglot)

;;;; Lisp
(use-package lispy)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode))

;;;; Organization
(use-package outshine)

;;;; Snippets
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
  (panda-leader-def "y" 'ivy-yasnippet))

;;; Language Modes
;;;; Assembly
(defun panda-setup-asm-mode ()
  (asmfmt-on-save-mode 1)
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
  (clang-format-on-save-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1)
  (c-set-style "linux")
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'panda-setup-c-mode)
(add-hook 'c++-mode-hook #'panda-setup-c-mode)

;;;; CMake
(defun panda-setup-cmake-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package cmake-mode
  :config
  (add-hook 'cmake-mode-hook #'panda-setup-cmake-mode))

;;;; Clojure
(defun panda-setup-clojure-mode ()
  (lispy-mode 1)
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'panda-setup-clojure-mode))

(use-package cider
  :config
  (add-hook 'cider-mode-hook (lambda ()
                               (interactive)
                               (company-mode 1))))

;;;; Common Lisp
(defun panda-setup-slime-mode ()
  (lispy-mode 1)
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package slime
  :config
  (add-hook 'slime-mode-hook #'panda-setup-slime-mode)
  (setq inferior-lisp-program (executable-find "sbcl"))
  (slime-setup '(slime-fancy)))

;;;; D
(defun panda-setup-d-mode ()
  (company-mode 1)
  (dfmt-on-save-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1))

(use-package d-mode
  :config
  (add-to-list 'eglot-server-programs '(d-mode . ("~/.dub/packages/.bin/dls-latest/dls")))
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

;;;; HTML / PHP / ASP.NET / Embedded Ruby
(defun panda-setup-web-mode ()
  (prettier-html-on-save-mode 1)
  (yas-minor-mode 1))

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
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

;;;; Makefile
(defun panda-setup-makefile-mode ()
  (panda-generic-format-on-save))

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
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package powershell
  :config
  (add-hook 'powershell-mode-hook #'panda-setup-powershell-mode))

;;;; Python
(defun panda-setup-python-mode ()
  (black-on-save-mode 1)
  (company-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1)
  (setq-local yas-indent-line 'fixed)
  (setq-local yas-also-auto-indent-first-line nil))

(use-package python
  :config
  (add-hook 'python-mode-hook #'panda-setup-python-mode)
  (setq python-indent-offset 4))

;;;; R
(defun panda-setup-r-mode ()
  (company-mode 1)
  (eglot-ensure)
  (styler-on-save-mode 1)
  (yas-minor-mode 1))

(use-package ess
  :commands R
  :custom
  (ess-use-flymake nil)
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
