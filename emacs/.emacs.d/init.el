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
(setq use-package-always-ensure t)

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

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

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
(defconst panda-neon-green "#39FF14")
(defconst panda-light-blue "#67C8FF")
(defconst panda-deep-saffron "#FF9933")

(defvar panda-exchange-region nil
  "First region to exchange via `panda-exchange-regions'.
nil if no exchange is in process, and a list (region-start region-end) otherwise.")

(defun panda-exchange-regions (beg-1 end-1 beg-2 end-2 &optional arg)
  "Exchanges two regions. Cancel a pending exchange if `arg' is provided."
  (interactive
   (cond
    (current-prefix-arg (list nil nil nil nil current-prefix-arg))
    ((region-active-p)
     (let ((region-bounds (list (region-beginning) (region-end))))
       (if panda-exchange-region
           (append panda-exchange-region region-bounds (list current-prefix-arg))
         (append region-bounds (list nil nil) (list current-prefix-arg)))))
    (t (user-error "No active region"))))
  (cond
   ;; arg provided
   (arg (if panda-exchange-region
            (progn
              (setq panda-exchange-region nil)
              (message "Exchange aborted"))
          (message "No exchange in process")))
   ;; first call
   ((eq beg-2 nil) (setq panda-exchange-region (list beg-1 end-1)))
   ;; second call
   (t (progn
        (if (or (<= end-1 beg-2) (<= end-2 beg-1)) ; regions are valid
            (destructuring-bind
                (beg-1 end-1 beg-2 end-2)
                (-sort #'< (list beg-1 end-1 beg-2 end-2))
              (let ((first-region-contents (buffer-substring beg-1 end-1))
                    (second-region-contents (buffer-substring beg-2 end-2)))
                (save-excursion
                  (goto-char beg-2)
                  (delete-region beg-2 end-2)
                  (insert first-region-contents)
                  (goto-char beg-1)
                  (delete-region beg-1 end-1)
                  (insert second-region-contents))))
          (message "Regions overlap"))
        (setq panda-exchange-region nil))))
  (deactivate-mark))

(bind-key "C-c x" #'panda-exchange-regions)
(bind-key [remap zap-to-char] #'zap-up-to-char)

(defun panda-end-isearch-forward ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(add-to-list 'isearch-mode-end-hook #'panda-end-isearch-forward)

;;;;; Help
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 1.0))

;;; Global Packages
;;;; Multi-Purpose
(use-package flx :defer t)
(use-package smex :defer t)

(use-package ivy
  :demand t
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
  :demand t
  :bind (("C-c r" . counsel-rg))
  :config
  (counsel-mode 1))

;;;; Executing Code
(use-package quickrun
  :bind (("C-c q" . quickrun)
         ("C-c Q" . quickrun-shell)))

(use-package realgud
  :defer t)

;;;; Editing
(use-package expand-region
  :bind (("C-." . er/expand-region)))

(use-package smartparens
  :demand t
  :bind (:map smartparens-mode-map
              ;; movement
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-M-e" . sp-up-sexp)
              ;; sexp operations
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-;" . sp-slurp-hybrid-sexp)
              ("C-M-," . sp-backward-slurp-sexp)
              ("C-M-<" . sp-backward-barf-sexp)
              ("C-M-." . sp-forward-slurp-sexp)
              ("C-M->" . sp-forward-barf-sexp)
              ;; other
              ("M-<backspace>" . sp-change-inner)
              ("M-S-<backspace>" . sp-unwrap-sexp)
              ("C-M-<backspace>" . sp-rewrap-sexp))
  :init
  (setq-default sp-escape-quotes-after-insert nil)
  :config
  (defun panda-map-sp-strict-keys ()
    "Maps the keys in `smartparens-strict-mode-map' directly
instead of through remaps. This allows them to apply to other
modes such as `c-mode', which defines its own functions."
    (catch 'break-loop
      (dolist (sp-map-elem smartparens-strict-mode-map)
        ;; find remap element
        (when (and (listp sp-map-elem) (eq (car sp-map-elem) 'remap))
          (dolist (remap-cons (nthcdr 2 sp-map-elem))
            (destructuring-bind (original-func . new-func) remap-cons
              (dolist (key (where-is-internal original-func))
                (define-key smartparens-strict-mode-map key new-func))))
          (throw 'break-loop t)))))
  (require 'smartparens-config)
  (panda-map-sp-strict-keys)
  (smartparens-global-mode 1)
  (smartparens-global-strict-mode 1))

(use-package undo-propose
  :bind (("C-?" . undo-propose)))

;;;; Git
(use-package magit
  :bind (("C-c g" . magit-status))
  :init
  (setq magit-auto-revert-mode nil))

(use-package git-timemachine
  :bind (("C-c t" . git-timemachine)))

;;;; Navigation
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground panda-neon-green))

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
  :bind-keymap (("C-c p" . projectile-command-map))
  :init
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;;; Windows
(use-package eyebrowse
  :bind (("C-c w s" . eyebrowse-switch-to-window-config)
         ("C-c w c" . panda-eyebrowse-create-window-config)
         ("C-c w t" . eyebrowse-rename-window-config)
         ("C-c w k" . eyebrowse-close-window-config))
  :init
  (defvar eyebrowse-mode-map (make-sparse-keymap))
  :config
  (defun panda-eyebrowse-create-window-config ()
    (interactive)
    (let ((tag (read-string "Window Config Tag: ")))
      (eyebrowse-create-window-config)
      (let ((created-config (eyebrowse--get 'current-slot)))
        (eyebrowse-rename-window-config created-config tag))))
  (eyebrowse-mode 1))

(use-package winner
  :config
  (winner-mode 1))

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

(use-package yasnippet-snippets
  :after yasnippet)

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
  :defer t
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
  :defer t
  :config
  (add-hook 'cmake-mode-hook #'panda-setup-cmake-mode))

;;;; Common Lisp
(defalias 'panda-setup-common-lisp-mode 'panda-setup-emacs-lisp-mode)

(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :config
  (add-hook 'slime-mode-hook #'panda-setup-common-lisp-mode)
  (slime-setup))

(use-package slime-company
  :after slime
  :config
  (slime-company-init))

;;;; D
(defun panda-setup-d-mode ()
  (company-mode 1)
  (dfmt-on-save-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1))

(use-package d-mode
  :defer t
  :config
  (add-to-list 'eglot-server-programs '(d-mode . ("dls")))
  (add-hook 'd-mode-hook #'panda-setup-d-mode))

;;;; Emacs Lisp
(defun panda-setup-emacs-lisp-mode ()
  (company-mode 1)
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'panda-setup-emacs-lisp-mode)

;;;; Git Files
(defun panda-setup-gitfiles-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(use-package gitattributes-mode
  :defer t
  :config
  (add-hook 'gitattributes-mode-hook #'panda-setup-gitfiles-mode))

(use-package gitconfig-mode
  :defer t
  :config
  (add-hook 'gitconfig-mode-hook #'panda-setup-gitfiles-mode))

(use-package gitignore-mode
  :defer t
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
  :defer t
  :config
  (add-hook 'go-mode-hook #'panda-setup-go-mode))

;;;; Haskell
(defun panda-setup-haskell-mode ()
  (brittany-on-save-mode 1)
  (company-mode 1)
  (eglot-ensure)
  (yas-minor-mode 1))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'panda-setup-haskell-mode))

;;;; HTML
(defun panda-setup-web-mode ()
  (prettier-html-on-save-mode 1)
  (yas-minor-mode 1))

(use-package web-mode
  :defer t
  :mode (("\\.html?\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4)
  :config
  (add-hook 'web-mode-hook #'panda-setup-web-mode))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

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
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'panda-setup-javascript-mode))

(use-package indium
  :defer t)

;;;; Latex
(defun panda-setup-latex-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'LaTeX-mode-hook #'panda-setup-latex-mode)

(use-package tex
  :ensure auctex
  :defer t
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
  :defer t
  :config
  (add-hook 'markdown-mode-hook #'panda-setup-markdown-mode))

;;;; Org
(defun panda-setup-org-mode ()
  (panda-generic-format-on-save))

(use-package org
  :defer t
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
  :defer t
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
  :defer t
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
  :defer t
  :config
  (add-hook 'rust-mode-hook #'panda-setup-rust-mode))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;;; Shell Script
(defun panda-setup-sh-mode ()
  (panda-generic-format-on-save)
  (yas-minor-mode 1))

(add-hook 'sh-mode-hook #'panda-setup-sh-mode)

;;;; TypeScript
(defun panda-setup-typescript-mode ()
  (company-mode 1)
  (eglot-ensure)
  (prettier-typescript-on-save-mode 1)
  (yas-minor-mode 1))

(use-package typescript-mode
  :defer t
  :config
  (add-hook 'typescript-mode-hook #'panda-setup-typescript-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (outshine-mode 1)
;; End:
