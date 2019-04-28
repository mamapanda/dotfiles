;;; Init.el Setup  -*- lexical-binding: t -*-
;;;; Package Management
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

;;;; Extra Files
(use-package no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Libraries
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
  (setq evil-move-beyond-eol    t
        evil-move-cursor-back   nil
        evil-symbol-word-search t
        evil-toggle-key         "C-s-+"
        evil-want-C-d-scroll    t
        evil-want-C-u-scroll    t
        evil-want-keybinding    nil
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
        evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

;;; Basic Configuration
;;;; Appearance
(setq default-frame-alist '((fullscreen . maximized)
                            (font . "Consolas-11")
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil))
      inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell nil)

(cl-defmacro panda-run-after-frame (&body body)
  "If a daemon is running, then add BODY to `after-make-frame-functions'
with a lambda wrapper. Else, simply evaluate BODY."
  (declare (indent defun))
  (if (daemonp)
      `(add-to-list 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame frame
                        ,@body)))
    `(progn ,@body)))

;;;; Behavior
(setq auto-save-default nil
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup"))
      delete-by-moving-to-trash t
      disabled-command-function nil
      inhibit-compacting-font-caches t
      make-backup-files nil
      recentf-max-saved-items 100
      save-abbrevs nil
      vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode t)
(recentf-mode 1)
(show-paren-mode 1)

;;;; Definitions
;;;;; Defuns
(defun panda-find-init-file (&optional arg)
  "Open `user-init-file'. If ARG is non-nil, open it in another window."
  (interactive "P")
  (if arg
      (find-file-other-window user-init-file)
    (find-file user-init-file)))

(defun panda-reload-file ()
  "Reload the current file, preserving point."
  (interactive)
  (if buffer-file-name
      (let ((pos (point)))
        (find-alternate-file buffer-file-name)
        (goto-char pos))
    (message "Buffer is not visiting a file")))

(defun panda-format-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

;;;;; Macros
(defmacro panda-add-hook-once (hook fn &optional append local)
  "Same as `add-hook', but FN is immediately removed from HOOK after
it has been run once."
  (let ((hook-fn-name (gensym)))
    `(progn
       (defun ,hook-fn-name ()
         (funcall ,fn)
         (remove-hook ,hook (quote ,hook-fn-name) ,local))
       (add-hook ,hook (quote ,hook-fn-name) ,append ,local))))

;;;;; Minor Modes
(define-minor-mode panda-format-on-save-mode
  "Indents a buffer and trims whitespace on save."
  :init-value nil
  :lighter "panda-format"
  (if panda-format-on-save-mode
      (add-hook 'before-save-hook #'panda-format-buffer nil t)
    (remove-hook 'before-save-hook #'panda-format-buffer t)))

(define-minor-mode panda-trim-on-save-mode
  "Trims whitespace on save."
  :init-value nil
  :lighter "panda-trim"
  (if panda-trim-on-save-mode
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t)))

;;;;; Operators
(evil-define-operator panda-query-replace (beg end type)
  "Evil operator for `query-replace'."
  (save-excursion
    (unless (evil-visual-state-p)
      (evil-visual-select beg end type))
    (call-interactively #'query-replace)))

;;;; Keybindings
(defun panda-bind-swap-key (fn)
  "Bind FN to backspace for temporary easy access."
  (interactive "CCommand: ")
  (general-nvmap "<backspace>" fn))

(panda-leader-def
  "SPC"         'execute-extended-command
  "b"           'switch-to-buffer
  "B"           'kill-buffer
  "f"           'find-file
  "k"           'previous-error
  "j"           'next-error
  "%"           (general-key "C-x C-q")
  "<backspace>" 'panda-bind-swap-key)

(general-nmap
  "Q" 'save-buffer
  "_" 'eval-expression)

(general-def [remap backward-kill-word] 'evil-delete-backward-word)

;;; Global Packages
;;;; Appearance
(use-package base16-theme
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (panda-run-after-frame
    (load-theme 'base16-oceanicnext t)))

(use-package display-line-numbers
  :general
  (panda-leader-def "l" 'panda-toggle-line-numbers)
  :init
  (setq-default display-line-numbers-type 'visual)
  :config
  (defun panda-toggle-line-numbers ()
    "Toggle between relative and absolute line numbers in current buffer."
    (interactive)
    (setq-local display-line-numbers-type (case display-line-numbers-type
                                            (visual t)
                                            ((t) 'visual)
                                            (otherwise 'visual)))
    (display-line-numbers-mode 1))
  (panda-run-after-frame
    (global-display-line-numbers-mode 1))
  (column-number-mode 1))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon nil)
  :config
  (panda-run-after-frame
    (set-face-attribute 'doom-modeline-bar nil
                        :background (face-attribute 'mode-line :background))
    (set-face-attribute 'doom-modeline-inactive-bar nil
                        :background (face-attribute 'mode-line-inactive :background)))
  (doom-modeline-init))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Editing
(use-package evil-args
  :general
  (general-itomap "a" 'evil-inner-arg)
  (general-otomap "a" 'evil-outer-arg))

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

(use-package replace
  :ensure nil
  :general
  (panda-leader-def "o" 'occur)
  (general-nmap :keymaps 'occur-mode-map
    "C-k"        'panda-occur-prev
    "C-j"        'panda-occur-next
    "C-<return>" 'occur-mode-display-occurrence)
  :config
  (progn
    (defun panda-occur-prev (&optional count)
      "Execute `occur-prev', then call `occur-mode-display-occurrence'."
      (interactive "p")
      (occur-prev count)
      (occur-mode-display-occurrence))
    (defun panda-occur-next (&optional count)
      "Execute `occur-next', then call `occur-mode-display-occurrence'."
      (interactive "p")
      (occur-next count)
      (occur-mode-display-occurrence)))
  (progn
    (require 'hl-line)
    (defvar panda--occur-overlay (let ((ov (make-overlay 0 0)))
                                   (overlay-put ov 'face 'hl-line)
                                   (delete-overlay ov)
                                   ov)
      "The overlay for the current occur match line.")
    (defun panda--set-occur-overlay ()
      "Move `panda--occur-overlay' to the current line and set it to
be deleted on `post-command-hook'."
      (move-overlay panda--occur-overlay
                    (line-beginning-position)
                    (1+ (line-end-position))
                    (current-buffer))
      (panda-add-hook-once 'post-command-hook
                           (lambda () (delete-overlay panda--occur-overlay))
                           nil t))
    (add-hook 'occur-mode-find-occurrence-hook #'panda--set-occur-overlay)
    (add-hook 'occur-mode-find-occurrence-hook #'recenter)))

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

;;;; Help
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;;;; Navigation
(use-package dired-filter)

(use-package dired-open
  :general
  (general-nmap :keymaps 'dired-mode-map
    "<C-return>" 'dired-open-xdg))

(use-package dired-sidebar
  :general
  (panda-leader-def
    "d" 'dired-sidebar-toggle-sidebar
    "D" 'dired)
  :init
  (setq dired-sidebar-theme 'none))

(use-package dired-subtree
  :general
  (general-nmap :keymaps 'dired-mode-map
    "<tab>" 'dired-subtree-cycle))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :init
  (setq evil-snipe-repeat-keys  nil
        evil-snipe-smart-case   t
        evil-snipe-scope        'visible
        evil-snipe-repeat-scope 'visible)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-visualstar
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode 1))

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

;;;; Shell
(use-package eshell
  :gfhook ('eshell-first-time-mode-hook 'panda--set-eshell-keys)
  :general
  (panda-leader-def "<return>" 'eshell)
  :config
  (defun panda--set-eshell-keys ()
    "Set keys for `eshell-mode'."
    (general-imap :keymaps 'eshell-mode-map
      "C-r" 'eshell-previous-matching-input
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input)))

;;;; UI Completion
(use-package flx)
(use-package smex)

(use-package ivy
  :general
  (general-def :keymaps 'ivy-minibuffer-map
    "<return>"   'ivy-alt-done
    "C-<return>" 'ivy-immediate-done)
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
    "F"   'counsel-recentf
    "s"   'counsel-rg)
  :config
  (counsel-mode 1))

;;;; Windows
(use-package eyebrowse
  :general
  (panda-leader-def
    "<tab>" 'eyebrowse-last-window-config
    "w"     'eyebrowse-switch-to-window-config
    "W"     'eyebrowse-close-window-config
    "e"     'panda-eyebrowse-create-window-config
    "E"     'eyebrowse-rename-window-config)
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
  :init
  (setq flycheck-display-errors-delay 0.5))

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

;;;; Language Server
(use-package lsp-mode
  :init
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-prefer-flymake nil))

(use-package company-lsp)

(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-diagnostics nil))

(use-package dap-mode
  :init
  (setq dap-utils-extension-path (no-littering-expand-var-file-name "dap"))
  :config
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-gdb-lldb)
  (require 'dap-go)
  (require 'dap-python)
  ;; workaround because `dap--breakpoints-file' is declared with `defconst'
  (setq dap--breakpoints-file (no-littering-expand-var-file-name "dap/breakpoints"))
  (dap-mode 1)
  (dap-ui-mode 1))

;;;; Lisp
(use-package lispyville
  :config
  (lispyville-set-key-theme '(operators))
  (with-eval-after-load 'evil-goggles
    (dolist (operators '((evil-yank . lispyville-yank)
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
        (panda-evil-goggles-add lispyville-operator evil-operator)))))

;;;; Organization
(use-package outshine
  :general
  (general-nmap :keymaps 'outshine-mode-map
    "<tab>" (lookup-key outshine-mode-map (kbd "TAB"))))

;;;; Snippets
(use-package yasnippet
  :init
  (setq yas-triggers-in-field nil
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  :config
  (yas-reload-all)
  (with-eval-after-load 'company
    (defun panda--company-yas-tab-advice (old-func &rest args)
      (unless (and (bound-and-true-p yas-minor-mode) (yas-expand))
        (call-interactively old-func args)))
    (when-let ((company-tab-func (lookup-key company-active-map (kbd "<tab>"))))
      (advice-add company-tab-func :around #'panda--company-yas-tab-advice))))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :general
  (panda-leader-def "y" 'ivy-yasnippet))

;;;; Macros
;;;;; Code Formatters
(use-package reformatter)

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
(use-package asm-mode
  :gfhook '(panda-set-asm-locals yas-minor-mode)
  :init
  (setq asm-comment-char ?#)
  :config
  (defun panda-set-asm-locals ()
    (setq-local indent-tabs-mode t)
    (setq-local tab-always-indent (default-value 'tab-always-indent)))
  (panda-formatter-def asmfmt
    :mode asm-mode
    :program "asmfmt"))

;;;; C / C++
(use-package cc-mode
  :gfhook ('(c-mode-hook c++-mode-hook) '(lsp panda-set-c-locals yas-minor-mode))
  :config
  (defun panda-set-c-locals ()
    (c-set-offset 'innamespace 0))
  (panda-formatter-def clang-format
    :mode (c-mode c++-mode)
    :program "clang-format"))

(use-package ccls)

;;;; CMake
(use-package cmake-mode
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

;;;; Common Lisp
(use-package lisp-mode
  :ensure nil
  :gfhook '(company-mode
            lispyville-mode
            panda-format-on-save-mode
            panda-set-lisp-locals
            yas-minor-mode)
  :config
  (defun panda-set-lisp-locals ()
    (setq-local evil-args-delimiters '(" "))))

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

;;;; Config
(use-package conf-mode
  :gfhook ('conf-unix-mode-hook 'panda-trim-on-save-mode))

;;;; D
(use-package d-mode
  :gfhook '(lsp yas-minor-mode)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("dls"))
                    :major-modes '(d-mode)
                    :server-id 'dls))
  (panda-formatter-def dfmt
    :mode d-mode
    :program "dfmt"
    :extra-args ("--brace_style=otbs" "--space_after_cast=false" "--max_line_length=80")
    :config-file ".editorconfig"))

;;;; Emacs Lisp
(use-package elisp-mode
  :ensure nil
  :gfhook ('emacs-lisp-mode-hook '(company-mode
                                   lispyville-mode
                                   panda-format-on-save-mode
                                   panda-set-elisp-locals
                                   yas-minor-mode))
  :config
  (defun panda-set-elisp-locals ()
    (setq-local evil-args-delimiters '(" ")))
  (dolist (map '(emacs-lisp-mode-map lisp-interaction-mode-map))
    (panda-setup-repl map
      :eval-line-or-expression 'eval-last-sexp
      :eval-region 'eval-region
      :eval-paragraph nil
      :eval-function 'eval-defun
      :eval-buffer 'eval-buffer)))

;;;; Fish
(use-package fish-mode
  :gfhook '(panda-trim-on-save-mode yas-minor-mode))

;;;; Git Files
(use-package gitattributes-mode
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

(use-package gitconfig-mode
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

(use-package gitignore-mode
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

;;;; Go
(use-package go-mode
  :gfhook '(lsp panda-set-go-locals yas-minor-mode)
  :config
  (defun panda-set-go-locals ()
    (setq-local indent-tabs-mode t))
  (panda-formatter-def gofmt
    :mode go-mode
    :program "gofmt"))

;;;; HTML / CSS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :gfhook '(lsp)
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4)
  :config
  (panda-formatter-def prettier-html
    :mode web-mode
    :program "prettier"
    :required-args ("--stdin" "--parser" "html")
    :config-file ".prettierrc"))

(use-package css-mode
  :gfhook '(lsp)
  :config
  (panda-formatter-def prettier-css
    :mode css-mode
    :program "prettier"
    :required-args ("--stdin" "--parser" "css")
    :extra-args ("--tab-width" "4")
    :config-file ".prettierrc"))

(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

;;;; JavaScript / TypeScript
(use-package js
  :gfhook '(yas-minor-mode))

(use-package typescript-mode
  :gfhook '(yas-minor-mode))

(panda-formatter-def prettier-ts
  :mode (js-mode typescript-mode)
  :program "prettier"
  :required-args ("--stdin" "--parser" "typescript")
  :extra-args ("--tab-width" "4")
  :config-file ".prettierrc")

(use-package tide
  :hook ((js-mode typescript-mode) . panda-enable-tide)
  :config
  (defun panda-enable-tide ()
    (company-mode 1)
    (flycheck-mode 1)
    (tide-setup)
    (tide-hl-identifier-mode 1)))

;;;; JSON
(use-package json-mode
  :gfhook '(panda-disable-js-hooks)
  :config
  (defun panda-disable-js-hooks ()
    (company-mode -1)
    (flycheck-mode -1)
    (prettier-ts-on-save-mode -1)
    (tide-mode -1)
    (tide-hl-identifier-mode -1))
  (panda-formatter-def prettier-json
    :mode json-mode
    :program "prettier"
    :required-args ("--stdin" "--parser" "json")
    :extra-args ("--tab-width" "4")
    :config-file ".prettierrc"))

;;;; Latex
(use-package tex
  :ensure auctex
  :gfhook ('LaTeX-mode-hook '(panda-format-on-save-mode yas-minor-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t))

;;;; Makefile
(use-package make-mode
  :gfhook ('makefile-mode-hook  '(panda-trim-on-save-mode yas-minor-mode)))

;;;; Markdown
(use-package markdown-mode
  :gfhook '(yas-minor-mode)
  :config
  (panda-formatter-def prettier-markdown
    :mode markdown-mode
    :program "prettier"
    :required-args ("--stdin" "--parser" "markdown")
    :config-file ".prettierrc"))

;;;; Org
(use-package org
  :gfhook '(panda-trim-on-save-mode)
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
(use-package python
  :gfhook '(lsp panda-set-python-locals yas-minor-mode)
  :init
  (setq python-indent-offset 4)
  :config
  (defun panda-set-python-locals ()
    (setq-local yas-indent-line 'fixed)
    (setq-local yas-also-auto-indent-first-line nil))
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
    :config-file "pyproject.toml"))

;;;; R
(use-package ess
  :gfhook ('ess-r-mode-hook '(panda-format-on-save-mode lsp yas-minor-mode))
  :init
  (setq ess-ask-for-ess-directory nil
        ess-use-flymake nil)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     '("R" "--slave" "-e" "languageserver::run()"))
                    :major-modes '(ess-r-mode)
                    :server-id 'R))
  (panda-setup-repl 'ess-r-mode-map
    :eval-line-or-expression 'ess-eval-line
    :eval-region 'ess-eval-region
    :eval-paragraph 'ess-eval-paragraph
    :eval-function 'ess-eval-function
    :eval-buffer 'ess-eval-buffer))

;;;; Rust
(use-package rust-mode
  :gfhook '(lsp yas-minor-mode)
  :config
  (panda-formatter-def rustfmt
    :mode rust-mode
    :program "rustfmt"))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;;; Shell Script
(use-package sh-script
  :gfhook ('sh-mode-hook '(panda-format-on-save-mode yas-minor-mode)))

;;;; YAML
(use-package yaml-mode
  :gfhook '(panda-trim-on-save-mode yas-minor-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; eval: (outshine-cycle-buffer)
;; End:
