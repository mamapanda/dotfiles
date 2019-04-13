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
;;;; Completion / Linting
(use-package company
  :general
  (general-def :keymaps 'company-active-map
    "C-p"      'company-select-previous
    "C-n"      'company-select-next
    "C-b"      'company-previous-page
    "C-f"      'company-next-page
    "<return>" 'company-complete-selection
    "C-g"      'company-abort
    "<escape>" 'company-abort)
  :init
  (setq company-dabbrev-code-modes nil
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  (defvar company-active-map (make-sparse-keymap))
  :config
  (delete 'company-dabbrev company-backends))

(use-package flycheck
  :general
  (panda-leader-def
    "j" 'flycheck-next-error
    "k" 'flycheck-previous-error))

;;;; Formatting
(defun panda-generic-format-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(use-package reformatter)

;;;; Language Server
(use-package lsp-mode
  :init
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-prefer-flymake nil))

(use-package company-lsp)
(use-package lsp-ui)

(use-package dap-mode
  :init
  (setq dap-utils-extension-path (panda-var-file "dap")
        dap--breakpoints-file (panda-var-file "dap/breakpoints"))
  :config
  (require 'dap-firefox)
  (require 'dap-gdb-lldb)
  (require 'dap-go)
  (require 'dap-python)
  (dap-mode 1)
  (dap-ui-mode 1))

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

;;;; Macros
;;;;; Language Setup
(defvar panda-language-features '((completion . (company-mode 1))
                                  (formatting . (add-hook
                                                 'before-save-hook
                                                 #'panda-generic-format-buffer
                                                 nil
                                                 t))
                                  (lsp . (lsp))
                                  (snippets . (yas-minor-mode 1))
                                  (trim-whitespace . (add-hook
                                                      'before-save-hook
                                                      #'delete-trailing-whitespace
                                                      nil
                                                      t)))
  "Maps a language feature symbol to its corresponding setup code.")

(defun panda-translate-language-feature (feature)
  "Converts a language feature symbol to its corresponding setup code."
  (let ((setup-code (alist-get feature panda-language-features)))
    (if setup-code
        setup-code
      (error "Language feature %s not defined" feature))))

(cl-defmacro panda-setup-language (mode
                                   &key
                                   features
                                   defaults
                                   locals
                                   lsp-server
                                   extra-code)
  "A macro to set up variables and hook functions for programming languages.

For each specified major mode, this macro:
- tries to install a package with the same name as MODE if MODE is not found
- sets default values for variables according to DEFAULTS
- creates a setup function based on FEATURES, LOCALS, and EXTRA-CODE
- adds the setup function to the major mode's hook
- registers a user-defined lsp server if one is provided

MODE is the name of the major mode of the programming language,
or a list of names if there is more than one.

FEATURES is a list of features defined in `panda-language-features'.
These features will be enabled for the specified major modes.

DEFAULTS is a list of items (symbol value) to be set as defaults.
This parameter is only here for organization.

LOCALS is a list of items (symbol value) to be set locally in MODE.

LSP-SERVER describes a user-defined lsp server in the form
(\"server-name\" \"--arg-1\" \"--arg-2\").

EXTRA-CODE is an extra statement to run at the end of the setup function."
  (declare (indent defun))
  (assert (or (symbolp mode) (listp mode)))
  (let ((mode-list (if (listp mode) mode (list mode))))
    `(progn
       ,@(mapcar (lambda (mode)
                   (unless (fboundp mode)
                     `(use-package ,mode)))
                 mode-list)
       ,(when lsp-server
          `(lsp-register-client
            (make-lsp-client :new-connection (lsp-stdio-connection ',lsp-server)
                             :major-modes ',mode-list
                             :server-id ',(intern (car lsp-server)))))
       ,@(mapcar (lambda (default)
                   (push 'setq-default default))
                 defaults)
       ,@(mapcar (lambda (mode)
                   (let ((fn-name (intern (format "panda-setup-%s" mode)))
                         (mode-hook (intern (format "%s-hook" mode))))
                     `(progn
                        (defun ,fn-name ()
                          ,@(mapcar #'panda-translate-language-feature features)
                          ,@(mapcar (lambda (local) (push 'setq-local local)) locals)
                          ,extra-code)
                        (add-hook ',mode-hook #',fn-name))))
                 mode-list))))

;;;;; Code Formatters
(cl-defmacro panda-formatter-def (name
                                  &key
                                  mode
                                  program
                                  required-args
                                  extra-args
                                  config-file)
  "Defines a formatter based on NAME, PROGRAM, REQUIRED-ARGS, and
EXTRA-ARGS and enables it to run on save in MODE. MODE may be a
single mode or a list of modes. Additionally, if CONFIG-FILE is found
in the current directory or one of its parents, then the formatter
program's arguments are locally set to REQUIRED-ARGS only."
  (declare (indent defun))
  (assert (symbolp name))
  (assert program)
  (let ((mode-list (if (listp mode) mode (list mode)))
        (args-name (intern (format "%s-args" name)))
        (setup-fn-name (intern (format "%s-setup" name)))
        (format-on-save-name (intern (format "%s-on-save-mode" name))))
    `(progn
       (defvar ,args-name
         ,(when-let (program-args (append required-args extra-args))
            `(quote ,program-args)))
       (reformatter-define ,name
         :program ,program
         :args ,args-name)
       (defun ,setup-fn-name ()
         (,format-on-save-name 1)
         ,(when config-file
            `(when (locate-dominating-file default-directory ,config-file)
               (setq-local ,args-name (quote ,required-args)))))
       ,@(mapcar (lambda (mode)
                   (let ((mode-hook (intern (format "%s-hook" mode))))
                     `(add-hook ',mode-hook #',setup-fn-name)))
                 mode-list))))

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

;;; Language Modes
;;;; Assembly
(panda-setup-language asm-mode
  :features (snippets)
  :defaults ((asm-comment-char ?#))
  :locals ((indent-tabs-mode t)
           (tab-always-indent (default-value 'tab-always-indent))))

(panda-formatter-def asmfmt
  :mode asm-mode
  :program "asmfmt")

;;;; C / C++
(panda-setup-language (c-mode c++-mode)
  :features (lsp snippets)
  :extra-code (c-set-offset 'innamespace 0))

(panda-formatter-def clang-format
  :mode (c-mode c++-mode)
  :program "clang-format")

(use-package ccls)

;;;; CMake
(panda-setup-language cmake-mode
  :features (formatting snippets))

;;;; D
(panda-setup-language d-mode
  :features (lsp snippets)
  :lsp-server ("dls"))

(panda-formatter-def dfmt
  :mode d-mode
  :program "dfmt"
  :extra-args ("--brace_style=otbs" "--space_after_cast=false" "--max_line_length=80")
  :config-file ".editorconfig")

;;;; Fish
(panda-setup-language fish-mode
  :features (snippets trim-whitespace))

;;;; Git Files
(panda-setup-language (gitattributes-mode gitconfig-mode gitignore-mode)
  :features (formatting snippets))

;;;; Go
(panda-setup-language go-mode
  :features (lsp snippets)
  :locals ((indent-tabs-mode t)))

(panda-formatter-def gofmt
  :mode go-mode
  :program "gofmt")

;;;; HTML / CSS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)))

(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

(panda-setup-language web-mode
  :defaults ((web-mode-markup-indent-offset 2)
             (web-mode-style-padding 4)
             (web-mode-script-padding 4)
             (web-mode-block-padding 4)))

(panda-formatter-def prettier-html
  :mode web-mode
  :program "prettier"
  :required-args ("--stdin" "--parser" "html")
  :config-file ".prettierrc")

(panda-formatter-def prettier-css
  :mode css-mode
  :program "prettier"
  :required-args ("--stdin" "--parser" "css")
  :extra-args ("--tab-width" "4")
  :config-file ".prettierrc")

;;;; JavaScript / TypeScript
(panda-setup-language (js-mode typescript-mode)
  :features (lsp snippets))

(panda-formatter-def prettier-js
  :mode (js-mode typescript-mode)
  :program "prettier"
  :required-args ("--stdin" "--parser" "typescript")
  :extra-args ("--tab-width" "4")
  :config-file ".prettierrc")

;;;; Latex
(use-package tex
  :ensure auctex)

(panda-setup-language LaTeX-mode
  :features (formatting snippets)
  :defaults ((TeX-auto-save t)
             (TeX-parse-self t)))

;;;; Lisp
(panda-setup-language (emacs-lisp-mode lisp-mode)
  :features (completion formatting snippets)
  :locals ((evil-args-delimiters '(" ")))
  :extra-code (lispyville-mode 1))

(dolist (map '(emacs-lisp-mode-map lisp-interaction-mode-map))
  (panda-setup-repl map
    :eval-line-or-expression 'eval-last-sexp
    :eval-region 'eval-region
    :eval-paragraph nil
    :eval-function 'eval-defun
    :eval-buffer 'eval-buffer))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :config
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

;;;; Makefile
(panda-setup-language makefile-mode
  :features (snippets trim-whitespace))

;;;; Markdown
(panda-setup-language markdown-mode
  :features (snippets))

(panda-formatter-def prettier-markdown
  :mode markdown-mode
  :program "prettier"
  :required-args ("--stdin" "--parser" "markdown")
  :config-file ".prettierrc")

;;;; Org
(use-package org
  :general
  (panda-leader-def "a" 'org-agenda)
  :init
  (setq org-agenda-files '("~/code/org/agenda.org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;; Python
(panda-setup-language python-mode
  :features (lsp snippets)
  :defaults ((python-indent-offset 4))
  :locals ((yas-indent-line 'fixed)
           (yas-also-auto-indent-first-line nil)))

(panda-setup-repl 'python-mode-map
  :eval-line-or-expression nil
  :eval-region 'python-shell-send-region
  :eval-paragraph nil
  :eval-function 'python-shell-send-defun
  :eval-buffer 'python-shell-send-buffer)

(panda-formatter-def black
  :mode python-mode
  :program "black"
  :required-args ("-" "--quiet")
  :extra-args ("--line-length" "80")
  :config-file "pyproject.toml")

;;;; R
(use-package ess
  :init
  (setq ess-ask-for-ess-directory nil
        ess-use-flymake nil))

(panda-setup-language ess-r-mode
  :features (formatting lsp snippets)
  :lsp-server ("R" "--slave" "-e" "languageserver::run()"))

(panda-setup-repl 'ess-r-mode-map
  :eval-line-or-expression 'ess-eval-line
  :eval-region 'ess-eval-region
  :eval-paragraph 'ess-eval-paragraph
  :eval-function 'ess-eval-function
  :eval-buffer 'ess-eval-buffer)

;;;; Rust
(panda-setup-language rust-mode
  :features (lsp snippets))

(panda-formatter-def rustfmt
  :mode rust-mode
  :program "rustfmt")

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;;; Shell Script
(panda-setup-language sh-mode
  :features (formatting snippets))

;;;; YAML
(panda-setup-language yaml-mode
  :features (snippets trim-whitespace))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; End:
