;;; Package Management

;; added by package.el
;; (package-initialize)

(require 'package)

(setq-default package-archives
              '(("gnu" . "https://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/"))
              package-archive-priorities
              '(("gnu" . 1)
                ("melpa" . 10)))

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

;;; Evil
(use-package general
  :config
  (general-override-mode)
  (general-evil-setup)
  (general-create-definer panda-override-evil
    :states '(normal operator motion visual)
    :keymaps 'override)
  (panda-override-evil
    :prefix "SPC"
    :prefix-map 'panda-leader-map)
  (general-create-definer panda-leader-def
    :keymaps 'panda-leader-map))

(use-package evil
  :init
  (setq evil-toggle-key "C-s-+"
        evil-want-C-d-scroll t
        evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t)
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (evil-mode 1))

(use-package evil-collection
  :config
  (delete 'company evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-escape
  :init
  (setq evil-escape-key-sequence "fd"
        evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

;;; Built-Ins Configuration
;;;; Appearance
(setq default-frame-alist '((fullscreen . maximized)
                            (font . "Consolas-11")
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil))
      inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil)

;;;; Behavior
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

;;;; Keybindings
(panda-leader-def
  "b"        'switch-to-buffer
  "B"        'kill-buffer
  "f"        'find-file
  "r"        'query-replace
  "o"        'occur
  "<return>" 'eshell)

(general-nmap
  "Q" 'save-buffer
  "U" 'read-only-mode)

(general-nmap :keymaps 'occur-mode-map
  "U" 'occur-edit-mode)

(general-nmap :keymaps 'occur-edit-mode-map
  "U" 'occur-cease-edit)

;;; Global Packages
;;;; Appearance
(use-package base16-theme
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (load-theme 'base16-oceanicnext t)
  ;; what??????
  (set-face-attribute 'line-number-current-line nil
                      :foreground (face-attribute 'line-number :background)
                      :background (face-attribute 'line-number :foreground)))

(use-package display-line-numbers
  :general
  (panda-leader-def "l" 'panda-toggle-line-numbers)
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
  (set-face-attribute 'doom-modeline-bar nil
                      :background (face-attribute 'mode-line :background))
  (set-face-attribute 'doom-modeline-inactive-bar nil
                      :background (face-attribute 'mode-line-inactive :background))
  (doom-modeline-init))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Multi-Purpose
(use-package flx)
(use-package smex)

(use-package ivy
  :general
  (general-def :keymaps 'ivy-minibuffer-map
    "<return>" 'ivy-alt-done)
  :init
  (setq ivy-wrap t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        confirm-nonexistent-file-or-buffer t
        ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package counsel
  :general
  (panda-leader-def
    "SPC" 'counsel-M-x
    "s" 'counsel-rg)
  :config
  (counsel-mode 1))

;;;; Help
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;;;; Editing
(use-package evil-args
  :general
  (general-def :keymaps 'evil-inner-text-objects-map
    "a" 'evil-inner-arg)
  (general-def :keymaps 'evil-outer-text-objects-map
    "a" 'evil-outer-arg))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-goggles
  :config
  (defun panda-evil-goggles-add (command based-on-command)
    (catch 'break-loop
      (dolist (cmd-config evil-goggles--commands)
        (when (eq (car cmd-config) based-on-command)
          (add-to-list 'evil-goggles--commands (cons command (cdr cmd-config)))
          (when (bound-and-true-p evil-goggles-mode)
            (evil-goggles-mode 1))
          (throw 'break-loop t)))))
  (evil-goggles-use-diff-refine-faces)
  (evil-goggles-mode 1))

(use-package evil-lion
  :config
  (evil-lion-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package expand-region
  :general
  (general-vmap "v" 'er/expand-region))

(use-package targets
  :quelpa (targets :fetcher github :repo "noctuid/targets.el")
  :config
  (targets-setup t))

(use-package undo-tree
  :general
  (panda-leader-def "u" 'undo-tree-visualize)
  :init
  (setq undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode))

;;;; Git
(use-package magit
  :general
  (panda-leader-def "g" 'magit-status)
  :init
  (setq magit-auto-revert-mode nil))

(use-package magit-todos
  :init
  (setq magit-todos-rg-extra-args '("--hidden" "--glob" "!.git/"))
  :config
  (magit-todos-mode))

(use-package evil-magit)

(use-package git-timemachine
  :general
  (panda-leader-def "G" 'git-timemachine))

;;;; Navigation
(use-package dired-sidebar
  :general
  (panda-leader-def
    "d" 'dired-sidebar-toggle-sidebar
    "D" 'dired)
  :init
  (setq dired-sidebar-theme 'none))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package imenu
  :general
  (panda-leader-def "i" 'imenu)
  :init
  (setq imenu-auto-rescan t))

(use-package projectile
  :general
  (panda-leader-def
    :prefix "p"
    :prefix-command 'projectile-command-map)
  :init
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;;; Windows
(use-package eyebrowse
  :general
  (panda-leader-def
    "w" 'eyebrowse-switch-to-window-config
    "W" 'eyebrowse-close-window-config
    "e" 'panda-eyebrowse-create-window-config
    "E" 'eyebrowse-rename-window-config)
  :init
  (defvar eyebrowse-mode-map (make-sparse-keymap))
  :config
  (defun panda-eyebrowse-create-window-config (tag)
    (interactive "sWindow Config Tag: ")
    (eyebrowse-create-window-config)
    (let ((created-config (eyebrowse--get 'current-slot)))
      (eyebrowse-rename-window-config created-config tag)))
  (eyebrowse-mode 1))

(use-package winner
  :general
  (panda-leader-def
    "q" 'winner-undo
    "Q" 'winner-redo)
  :config
  (winner-mode 1))

;;; Per-Language Configuration
;;;; Macros
;;;;; Repl Setup
(let ((no-repl-found-message
       (lambda (send-type)
         `(lambda ()
            (interactive)
            (user-error "No REPL send %s command found for %s"
                        ,send-type major-mode)))))
  (general-nmap
    :prefix "SPC"
    "Z" (funcall no-repl-found-message "paragraph")
    "z" (funcall no-repl-found-message "line/expression")
    "X" (funcall no-repl-found-message "buffer")
    "x" (funcall no-repl-found-message "function"))
  (general-vmap
    :prefix "SPC"
    "z" (funcall no-repl-found-message "region")))

(cl-defmacro panda-setup-repl (map
                               &key
                               eval-region
                               eval-line-or-expression
                               eval-paragraph
                               eval-function
                               eval-buffer)
  (declare (indent defun))
  `(progn
     (general-nmap :keymaps ,map
       :prefix "SPC"
       "Z" ,eval-paragraph
       "z" ,eval-line-or-expression
       "X" ,eval-buffer
       "x" ,eval-function)
     (general-vmap :keymaps ,map
       :prefix "SPC"
       "z" ,eval-region)))

;;;; Completion / Linting
(use-package company
  :general
  (general-def :keymaps 'company-active-map
    "C-p"      'company-select-previous
    "C-n"      'company-select-next
    "C-b"      'company-previous-page
    "C-f"      'company-next-page
    "<return>" 'company-complete-selection)
  :init
  (setq company-dabbrev-code-modes nil
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  (defvar company-active-map (make-sparse-keymap))
  :config
  (delete 'company-dabbrev company-backends))

(use-package flymake
  :general
  (general-nmap
    "\\" 'flymake-show-diagnostics-buffer)
  (panda-leader-def
    "k" 'flymake-goto-prev-error
    "j" 'flymake-goto-next-error))

;;;; Debugging
(use-package realgud)

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
    :program "clang-format")
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
    :args '("--stdin" "--parser" "typescript" "--tab-width" "4"))
  (panda-reformatter-define prettier-markdown
    :program "prettier"
    :args '("--stdin" "--parser" "markdown"))
  (panda-reformatter-define rustfmt
    :program "rustfmt"))

;;;; Language Server
(use-package eglot)

;;;; Lisp
(use-package lispyville
  :config
  (lispyville-set-key-theme '(operators))
  (eval-after-load 'evil-goggles
    (progn (dolist (operators '((evil-yank . lispyville-yank)
                                (evil-delete . lispyville-delete)
                                (evil-change . lispyville-change)
                                (evil-yank-line . lispyville-yank-line)
                                (evil-delete-line . lispyville-delete-line)
                                (evil-change-line . lispyville-change-line)
                                (evil-delete-char . lispyville-delete-char-or-splice)
                                (evil-delete-backward-char . lispyville-delete-char-or-splice-backwards)
                                (evil-substitute . lispyville-substitute)
                                (evil-change-whole-line . lispyville-change-whole-line)
                                (evil-join . lispyville-join)))
             (destructuring-bind (evil-operator . lispyville-operator) operators
               (panda-evil-goggles-add lispyville-operator evil-operator))))))

;;;; Organization
(use-package outshine)

;;;; Snippets
(use-package yasnippet
  :init
  (setq yas-triggers-in-field nil
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  :config
  (yas-reload-all)
  (eval-after-load 'company
    (progn
      (defun panda-company-yas-tab-advice (old-func &rest args)
        (unless (and (bound-and-true-p yas-minor-mode) (yas-expand))
          (call-interactively old-func args)))
      (when-let ((company-tab-func (lookup-key company-active-map (kbd "<tab>"))))
        (advice-add company-tab-func :around #'panda-company-yas-tab-advice)))))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
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
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :config
  (add-hook 'slime-mode-hook #'panda-setup-common-lisp-mode)
  (slime-setup)
  (panda-setup-repl 'slime-mode-map
    :eval-line-or-expression 'slime-eval-last-expression
    :eval-region 'slime-eval-region
    :eval-paragraph nil
    :eval-function 'slime-eval-defun
    :eval-buffer 'slime-eval-buffer))

(use-package slime-company
  :config
  (slime-company-init))

;;;; CSS
(defun panda-setup-css-mode ()
  (prettier-css-on-save-mode 1))

(add-hook 'css-mode-hook #'panda-setup-css-mode)

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
  (lispyville-mode 1)
  (panda-generic-format-on-save)
  (yas-minor-mode 1)
  (setq-local evil-args-delimiters '(" ")))

(dolist (map '(emacs-lisp-mode-map lisp-interaction-mode-map))
  (panda-setup-repl map
    :eval-line-or-expression 'eval-last-sexp
    :eval-region 'eval-region
    :eval-paragraph nil
    :eval-function 'eval-defun
    :eval-buffer 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'panda-setup-emacs-lisp-mode)

;;;; Fish
(defun panda-setup-fish-mode ()
  (panda-trim-whitespace-on-save)
  (yas-minor-mode 1))

(use-package fish-mode
  :config
  (add-hook 'fish-mode-hook #'panda-setup-fish-mode))

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
  (prettier-html-on-save-mode 1))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4)
  :config
  (add-hook 'web-mode-hook #'panda-setup-web-mode))

(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

;;;; Java
(defun panda-setup-java-mode ()
  (clang-format-on-save-mode 1)
  (yas-minor-mode 1))

(add-hook 'java-mode-hook #'panda-setup-java-mode)

;;;; JavaScript / TypeScript
(defun panda-setup-javascript-mode ()
  (company-mode 1)
  (eglot-ensure)
  (prettier-javascript-on-save-mode 1)
  (yas-minor-mode 1))

(use-package typescript-mode)
(add-to-list 'eglot-server-programs
             '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))

(add-hook 'js-mode-hook #'panda-setup-javascript-mode)
(add-hook 'typescript-mode-hook #'panda-setup-javascript-mode)

(use-package indium
  :config
  (panda-setup-repl 'indium-interaction-mode-map
    :eval-line-or-expression 'indium-eval-last-node
    :eval-region 'indium-eval-region
    :eval-paragraph nil
    :eval-function 'indium-eval-defun
    :eval-buffer 'indium-eval-buffer))

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
  :general
  (panda-leader-def "a" 'org-agenda)
  :init
  (setq org-agenda-files '("~/code/org/agenda.org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  :config
  (add-hook 'org-mode-hook #'panda-setup-org-mode))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
  (add-hook 'python-mode-hook #'panda-setup-python-mode)
  (panda-setup-repl 'python-mode-map
    :eval-line-or-expression nil
    :eval-region 'python-shell-send-region
    :eval-paragraph nil
    :eval-function 'python-shell-send-defun
    :eval-buffer 'python-shell-send-buffer))

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
  (add-hook 'ess-r-mode-hook #'panda-setup-r-mode)
  (panda-setup-repl 'ess-r-mode-map
    :eval-line-or-expression 'ess-eval-line
    :eval-region 'ess-eval-region
    :eval-paragraph 'ess-eval-paragraph
    :eval-function 'ess-eval-function
    :eval-buffer 'ess-eval-buffer))

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

;;;; Shell Script
(defun panda-setup-sh-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'sh-mode-hook #'panda-setup-sh-mode)

;;;; YAML
(defun panda-setup-yaml-mode ()
  (panda-trim-whitespace-on-save)
  (yas-minor-mode 1))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook #'panda-setup-yaml-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; End:
