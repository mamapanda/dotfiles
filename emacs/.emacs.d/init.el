;;; Init.el Setup  -*- lexical-binding: t -*-
;;;; cl-lib
(require 'cl-lib)

;;;; Startup Optimizations
(cl-defmacro panda-setq-init (symbol value &rest args)
  "Same as `setq', but only applies during initialization and restores
the original values of the variables in `after-init-hook'."
  (cl-labels ((build-form
               (symbol value args)
               (cons
                `(progn
                   (unless after-init-time
                     (setq ,symbol ,value))
                   (add-hook 'after-init-hook
                             (lambda ()
                               (setq ,symbol (quote ,(symbol-value symbol))))))
                (when args
                  (build-form (cl-first args) (cl-second args) (nthcdr 2 args))))))
    `(progn
       ,@(build-form symbol value args))))

(panda-setq-init gc-cons-threshold 64000000
                 file-name-handler-alist nil)

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
(setq use-package-always-ensure t)

(use-package quelpa
  :commands quelpa
  :init
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  :config
  (quelpa-use-package-activate-advice))

;;;; Libraries
(use-package hydra)
(use-package no-littering)

;;;; Custom File
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Evil
(use-package evil
  :init
  (setq evil-move-beyond-eol    t
        evil-symbol-word-search t
        evil-toggle-key         "C-s-+"
        evil-want-C-d-scroll    t
        evil-want-C-u-scroll    t
        evil-want-keybinding    nil
        evil-want-Y-yank-to-eol t)
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (evil-mode 1))

(use-package general
  :config
  (general-override-mode)
  (general-evil-setup)
  (general-create-definer panda-space
    :states '(normal operator motion visual)
    :keymaps 'override
    :prefix "SPC"
    :prefix-map 'panda-space-map)
  (general-create-definer panda-space-sc
    :states '(normal operator motion visual)
    :keymaps 'override
    :prefix "SPC ;"
    :prefix-map 'panda-space-sc-map))

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
      blink-cursor-blinks 0
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup"))
      delete-by-moving-to-trash t
      disabled-command-function nil
      inhibit-compacting-font-caches t
      make-backup-files nil
      recentf-max-saved-items 100
      require-final-newline t
      save-abbrevs nil
      vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(blink-cursor-mode 1)
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

(defun panda-format-buffer ()
  "Indent the entire buffer and delete trailing whitespace."
  (interactive)
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(defun panda-insert-space-before (&optional count)
  "Insert COUNT spaces before point."
  (interactive "P")
  (dotimes (i (or count 1))
    (insert " ")))

(defun panda-insert-space-after (&optional count)
  "Insert COUNT spaces after point."
  (interactive "P")
  (save-excursion
    (evil-forward-char)
    (dotimes (i (or count 1))
      (insert " "))))

(defun panda-insert-newline-before (&optional count)
  "Insert COUNT newlines before point and indent."
  (interactive "P")
  (dotimes (i (or count 1))
    (newline-and-indent)))

(defun panda-insert-newline-after (&optional count)
  "Insert COUNT newlines after point and indent."
  (interactive "P")
  (save-excursion
    (evil-forward-char)
    (dotimes (i (or count 1))
      (newline-and-indent))))

(defun panda-reload-file ()
  "Reload the current file, preserving point."
  (interactive)
  (if buffer-file-name
      (let ((pos (point)))
        (find-alternate-file buffer-file-name)
        (goto-char pos))
    (message "Buffer is not visiting a file")))

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

;;;;; Motions
(evil-define-motion panda-forward-defun (&optional count)
  "Move forward COUNT defuns."
  (let* ((count (or count 1))
         (defun-bounds (bounds-of-thing-at-point 'defun))
         (at-defun-begin (eq (point) (car defun-bounds)))
         ;; Emacs counts the ending newline as part of the defun,
         ;; but we don't want to include it.
         (at-defun-end (and (eq (point) (cdr defun-bounds))
                            (looking-back "\n"))))
    (cond ((> count 0)
           (progn
             (dotimes (i count)
               (end-of-defun))
             ;; We need to jump an extra time if we were in a defun.
             (when (and defun-bounds (not at-defun-end))
               (end-of-defun))
             (beginning-of-defun)))
          ((< count 0)
           (progn
             (dotimes (i (- count))
               (beginning-of-defun))
             ;; We need to jump an extra time if we were in a defun.
             (when (and defun-bounds (not at-defun-begin) (not at-defun-end))
               (beginning-of-defun)))))))

(evil-define-motion panda-backward-defun (&optional count)
  "Move backward COUNT defuns."
  (let ((count (or count 1)))
    (panda-forward-defun (- count))))

;;;;; Operators
(evil-define-operator panda-query-replace (beg end type)
  "Evil operator for `query-replace'."
  (save-excursion
    (unless (evil-visual-state-p)
      (evil-visual-select beg end type))
    (call-interactively #'query-replace)))

;;;;; Text Objects
;;;;;; Buffer
(evil-define-text-object panda-outer-buffer (&optional count beg end type)
  "Select the whole buffer."
  :type line
  (evil-range (point-min) (point-max)))

(defalias 'panda-inner-buffer 'panda-outer-buffer)

;;;;;; Defun
(evil-define-text-object panda-outer-defun (&optional count beg end type)
  "Select a function."
  :type line
  (cl-destructuring-bind (begin . end) (bounds-of-thing-at-point 'defun)
    (evil-range begin end)))

(defvar panda-inner-defun-begin-regexp "{"
  "Regexp representing the left delimiter for the inside of a function.
This should not include any mandatory newlines, as `panda-inner-defun'
will automatically skip over those.")

(defvar panda-inner-defun-end-regexp "}"
  "Regexp representing the right delimiter for the inside of a function.")

(defmacro panda-set-inner-defun-regexp (mode begin end)
  "Set `panda-inner-defun-begin-regexp' and `panda-inner-defun-end-regexp'
for MODE. MODE may be a symbol or a list of modes."
  (declare (indent defun))
  (let* ((mode-list (if (listp mode) mode (list mode)))
         (hook-names (mapcar (lambda (mode) (intern (format "%s-hook" mode))) mode-list)))
    `(general-add-hook (quote ,hook-names)
                       (lambda ()
                         (setq-local panda-inner-defun-begin-regexp ,begin)
                         (setq-local panda-inner-defun-end-regexp ,end)))))

(panda-set-inner-defun-regexp (emacs-lisp-mode lisp-interaction-mode lisp-mode) "(" ")")
(panda-set-inner-defun-regexp python-mode ":" "")

(evil-define-text-object panda-inner-defun (&optional count beg end type)
  "Select inside a function."
  (cl-destructuring-bind (defun-begin . (defun-end . _)) (panda-outer-defun)
    ;; require \n on both ends to make it linewise
    (let* ((make-linewise t)
           (begin (save-excursion
                    (goto-char defun-begin)
                    (re-search-forward panda-inner-defun-begin-regexp)
                    (if (looking-at "[[:blank:]]*\n")
                        (progn
                          (next-line)
                          (beginning-of-line))
                      (setq make-linewise nil))
                    (point)))
           (end (save-excursion
                  (goto-char defun-end)
                  (re-search-backward panda-inner-defun-end-regexp)
                  (if (looking-back "\n[[:blank:]]*")
                      (search-backward "\n")
                    (setq make-linewise nil))
                  (point))))
      (evil-range begin end (if make-linewise 'line type)))))

(evil-define-text-object panda-outer-last-defun (&optional count beg end type)
  (save-excursion
    (panda-backward-defun count)
    (panda-outer-defun)))

(evil-define-text-object panda-outer-next-defun (&optional count beg end type)
  (save-excursion
    (panda-forward-defun count)
    (panda-outer-defun)))

(evil-define-text-object panda-inner-last-defun (&optional count beg end type)
  (save-excursion
    (panda-backward-defun count)
    (panda-inner-defun)))

(evil-define-text-object panda-inner-next-defun (&optional count beg end type)
  (save-excursion
    (panda-forward-defun count)
    (panda-inner-defun)))

;;;; Keybindings
(defun panda-bind-swap-key (fn)
  "Bind FN to backspace for temporary easy access."
  (interactive "CCommand: ")
  (general-nvmap "<backspace>" fn))

(panda-space
  "SPC"         'execute-extended-command
  "b"           'switch-to-buffer
  "B"           'kill-buffer
  "d"           'dired
  "f"           'find-file
  "t"           'bookmark-jump
  "T"           'bookmark-set
  "k"           'previous-error
  "j"           'next-error
  "4"           (general-key "C-x 4")
  "%"           (general-key "C-x C-q")
  "<backspace>" 'panda-bind-swap-key)

(general-nmap :keymaps 'override
  "Q" 'save-buffer)

(general-nmap
  "_" 'eval-expression
  "[ SPC" 'panda-insert-space-before
  "] SPC" 'panda-insert-space-after
  "[ <return>" 'panda-insert-newline-before
  "] <return>" 'panda-insert-newline-after)

(general-imap "<C-backspace>" 'evil-delete-backward-word)

(general-mmap
  "[d" 'panda-backward-defun
  "]d" 'panda-forward-defun)

(general-otomap
  "e"  'panda-outer-buffer
  "d"  'panda-outer-defun
  "ld" 'panda-outer-last-defun
  "nd" 'panda-outer-next-defun)

(general-itomap
  "e"  'panda-inner-buffer
  "d"  'panda-inner-defun
  "ld" 'panda-inner-last-defun
  "nd" 'panda-inner-next-defun)

;;; Global Packages
;;;; Appearance
(use-package base16-theme
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (panda-run-after-frame
    (load-theme 'base16-oceanicnext t)))

(use-package default-text-scale
  :commands panda-zoom/body
  :config
  (defhydra panda-zoom (:hint nil)
    "zoom"
    ("+" default-text-scale-increase "in"   :color red)
    ("-" default-text-scale-decrease "out"  :color red)
    ("0" default-text-scale-reset    "reset":color blue))
  (default-text-scale-mode 1))

(use-package display-line-numbers
  :demand t
  :general
  (panda-space "l" 'panda-toggle-line-numbers)
  :init
  (setq-default display-line-numbers-type 'visual)
  :config
  (defun panda-toggle-line-numbers ()
    "Toggle between relative and absolute line numbers in current buffer."
    (interactive)
    (setq-local display-line-numbers-type (cl-case display-line-numbers-type
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
  (panda-space "o" 'occur)
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
  :demand t
  :general
  (panda-space "u" 'undo-tree-visualize)
  :init
  (setq undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode))

;;;; Help
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;;;; Navigation
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
  (panda-space "i" 'imenu)
  :init
  (setq imenu-auto-rescan t))

(use-package projectile
  :defer t
  :general
  (panda-space "p" '(:keymap projectile-command-map))
  :init
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;;; UI Completion
(use-package flx :defer t)
(use-package smex :defer t)

(use-package ivy
  :demand t
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

(use-package ivy-hydra
  :commands hydra-ivy/body)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer-other-window
                               'ivy-rich--ivy-switch-buffer-transformer)
  (with-eval-after-load 'counsel-projectile
    (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer
                                 'ivy-rich--ivy-switch-buffer-transformer))
  (ivy-rich-mode 1))

(use-package counsel
  :demand t
  :general
  (panda-space
    "F" 'counsel-recentf
    "s" 'counsel-git-grep
    "S" 'counsel-rg)
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after counsel projectile
  :config
  (counsel-projectile-mode 1))

(use-package helm
  :general
  (general-def :keymaps 'helm-map
    "<escape>" 'helm-keyboard-quit)
  (panda-space "m" 'helm-mini)
  :init
  (setq helm-echo-input-in-header-line        t
        helm-ff-fuzzy-matching                nil
        helm-find-files-ignore-thing-at-point t
        helm-split-window-inside-p            t
        helm-mini-default-sources             '(helm-source-buffers-list
                                                helm-source-recentf))
  :config
  (with-eval-after-load 'helm-ls-git
    (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-ls-git
                                      helm-source-recentf))))

(use-package helm-ls-git
  :after helm
  :config
  (setq helm-source-ls-git (helm-make-source "Git Files" 'helm-ls-git-source)))

;;;; Windows
(use-package eyebrowse
  :general
  (panda-space
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
  :demand t
  :general
  (panda-space
    "q" 'winner-undo
    "Q" 'winner-redo)
  :config
  (winner-mode 1))

;;; Tools
;;;; Dired
(use-package dired-filter
  :commands dired-filter-mode)

(use-package dired-open
  :general
  (general-nmap :keymaps 'dired-mode-map
    "<C-return>" 'dired-open-xdg))

(use-package dired-sidebar
  :general
  (panda-space "D" 'dired-sidebar-toggle-sidebar)
  :init
  (setq dired-sidebar-theme 'none))

(use-package dired-subtree
  :general
  (general-nmap :keymaps 'dired-mode-map
    "<tab>" 'dired-subtree-cycle))

;;;; File Viewers
(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :gfhook '(visual-line-mode)
  :init
  (setq nov-text-width most-positive-fixnum))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :gfhook ('pdf-view-mode-hook 'panda-set-pdf-locals)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (defun panda-set-pdf-locals ()
    (display-line-numbers-mode -1)
    (setq-local evil-default-cursor (list nil)))
  (pdf-tools-install))

;;;; Git
(use-package magit
  :general
  (panda-space "g" 'magit-status)
  :init
  (setq magit-auto-revert-mode nil))

(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-rg-extra-args '("--hidden" "--glob" "!.git/"))
  :config
  (magit-todos-mode))

(use-package evil-magit :after magit)

(use-package git-timemachine
  :general
  (panda-space "G" 'git-timemachine))

;;;; Package Managers
(use-package helm-system-packages
  :commands helm-system-packages)

;;;; Shells
(use-package eshell
  :gfhook ('eshell-first-time-mode-hook 'panda--set-eshell-keys)
  :general
  (panda-space "<return>" 'eshell)
  :config
  (defun panda--set-eshell-keys ()
    "Set keys for `eshell-mode'."
    (general-imap :keymaps 'eshell-mode-map
      "C-r" 'eshell-previous-matching-input
      "C-p" 'eshell-previous-matching-input-from-input
      "C-n" 'eshell-next-matching-input-from-input)))

(use-package shell-pop
  :general
  (panda-space
    "<return>"   'shell-pop
    "<S-return>" 'eshell)
  :init
  (setq shell-pop-full-span  t
        shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell)))))

;;; Per-Language Configuration
;;;; Completion / Linting
(use-package company
  :commands company-mode
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
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  (defvar company-active-map (make-sparse-keymap))
  :config
  (delete 'company-dabbrev company-backends))

(use-package flycheck
  :commands flycheck-mode
  :general
  (panda-space-sc :keymaps 'flycheck-mode-map
    "el" 'flycheck-list-errors
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "ev" 'flycheck-verify-setup)
  :init
  (setq flycheck-display-errors-delay 0.5))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

;;;; Language Server
(use-package lsp-mode
  :commands lsp lsp-register-client
  :general
  (panda-space-sc :keymaps 'lsp-mode-map
    "fr" 'lsp-find-references
    "rs" 'lsp-rename
    "\\" 'lsp-restart-workspace)
  :init
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-prefer-flymake nil))

(use-package company-lsp :after company lsp)

(use-package lsp-ui
  :after lsp
  :general
  (panda-space-sc :keymaps 'lsp-ui-mode-map
    "rc" 'lsp-ui-sideline-apply-code-actions)
  :init
  (setq lsp-ui-sideline-show-diagnostics nil))

(use-package dap-mode
  :general
  (panda-space-sc :keymaps 'lsp-mode-map
    "dd" 'dap-debug
    "dh" 'dap-hydra)
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
  :commands lispyville-mode
  :config
  (lispyville-set-key-theme '(commentary operators slurp/barf-cp))
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
      (cl-destructuring-bind (evil-operator . lispyville-operator) operators
        (panda-evil-goggles-add lispyville-operator evil-operator)))))

;;;; Organization
(use-package outshine
  :commands outshine-mode
  :init
  (setq outshine-org-style-global-cycling-at-bob-p t)
  :config
  (general-nmap :keymaps 'outshine-mode-map
    "<tab>"     (lookup-key outshine-mode-map (kbd "TAB"))
    "<backtab>" 'outshine-cycle-buffer))

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

(use-package yasnippet-snippets :after yasnippet)

(use-package ivy-yasnippet
  :after ivy yasnippet
  :general
  (panda-space "y" 'ivy-yasnippet))

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
  (cl-assert (symbolp name))
  (cl-assert program)
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

;;; Language Modes
;;;; Assembly
(use-package asm-mode
  :defer t
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
  :defer t
  :gfhook ('(c-mode-hook c++-mode-hook) '(lsp panda-set-c-locals yas-minor-mode))
  :config
  (defun panda-set-c-locals ()
    (c-set-offset 'innamespace 0))
  (panda-formatter-def clang-format
    :mode (c-mode c++-mode)
    :program "clang-format"))

(use-package ccls
  :hook ((c-mode c++-mode) . panda-ccls)
  :config
  (defun panda-ccls ()
    "Load \"ccls\", then call `lsp'."
    (require 'ccls)
    (lsp)))

;;;; CMake
(use-package cmake-mode
  :defer t
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

;;;; Common Lisp
(use-package lisp-mode
  :ensure nil
  :defer t
  :gfhook '(company-mode
            lispyville-mode
            panda-format-on-save-mode
            panda-set-lisp-locals
            yas-minor-mode)
  :config
  (defun panda-set-lisp-locals ()
    (setq-local evil-args-delimiters '(" "))))

(use-package slime
  :general
  (panda-space-sc :keymaps 'slime-mode-map
    "xb" 'slime-eval-buffer
    "xd" 'slime-eval-defun
    "xe" 'slime-eval-last-expression
    "xr" 'slime-eval-region
    "xx" 'slime)
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :config
  (slime-setup))

(use-package slime-company
  :after slime
  :config
  (slime-company-init))

;;;; Config
(use-package conf-mode
  :defer t
  :gfhook ('conf-unix-mode-hook 'panda-trim-on-save-mode))

;;;; D
(use-package d-mode
  :defer t
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
  :defer t
  :gfhook ('emacs-lisp-mode-hook '(company-mode
                                   lispyville-mode
                                   panda-format-on-save-mode
                                   panda-set-elisp-locals
                                   yas-minor-mode))
  :general
  (panda-space-sc :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "dm" 'pp-macroexpand-last-sexp
    "xb" 'eval-buffer
    "xd" 'eval-defun
    "xe" 'eval-last-sexp
    "xr" 'eval-region
    "xx" 'ielm)
  :config
  (defun panda-set-elisp-locals ()
    (setq-local evil-args-delimiters '(" "))))

;;;; Fish
(use-package fish-mode
  :defer t
  :gfhook '(panda-trim-on-save-mode yas-minor-mode))

;;;; Git Files
(use-package gitattributes-mode
  :defer t
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

(use-package gitconfig-mode
  :defer t
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

(use-package gitignore-mode
  :defer t
  :gfhook '(panda-format-on-save-mode yas-minor-mode))

;;;; Go
(use-package go-mode
  :defer t
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
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t
        web-mode-markup-indent-offset 2
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
  :defer t
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
  :defer t
  :gfhook '(lsp yas-minor-mode))

(use-package rjsx-mode :defer t)

(use-package typescript-mode
  :defer t
  :gfhook '(lsp yas-minor-mode))

(panda-formatter-def prettier-ts
  :mode (js-mode typescript-mode)
  :program "prettier"
  :required-args ("--stdin" "--parser" "typescript")
  :extra-args ("--tab-width" "4")
  :config-file ".prettierrc")

(use-package tide
  :disabled t
  :hook ((js-mode typescript-mode) . panda-enable-tide)
  :general
  (panda-space-sc :keymaps 'tide-mode-map
    "rs" 'tide-rename-symbol
    "\\" 'tide-restart-server)
  :config
  (defun panda-enable-tide ()
    (company-mode 1)
    (flycheck-mode 1)
    (tide-setup)
    (tide-hl-identifier-mode 1)))

;;;; JSON
(use-package json-mode
  :defer t
  :gfhook '(panda-disable-js-hooks)
  :config
  (defun panda-disable-js-hooks ()
    (company-mode -1)
    (flycheck-mode -1)
    (prettier-ts-on-save-mode -1))
  (panda-formatter-def prettier-json
    :mode json-mode
    :program "prettier"
    :required-args ("--stdin" "--parser" "json")
    :extra-args ("--tab-width" "4")
    :config-file ".prettierrc"))

;;;; Latex
(use-package tex
  :ensure auctex
  :defer t
  :gfhook ('LaTeX-mode-hook '(panda-format-on-save-mode yas-minor-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t))

;;;; Makefile
(use-package make-mode
  :defer t
  :gfhook ('makefile-mode-hook  '(panda-trim-on-save-mode yas-minor-mode)))

;;;; Markdown
(use-package markdown-mode
  :defer t
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
  (panda-space "a" 'org-agenda)
  :init
  (setq org-agenda-files '("~/code/org/agenda.org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(additional calendar insert navigation))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;; Python
(use-package python
  :defer t
  :gfhook '(lsp panda-set-python-locals yas-minor-mode)
  :general
  (panda-space-sc :keymaps 'python-mode-map
    "xb" 'python-shell-send-buffer
    "xd" 'python-shell-send-defun
    "xf" 'python-shell-send-file
    "xr" 'python-shell-send-region
    "xx" 'run-python)
  :init
  (setq python-indent-offset 4)
  :config
  (defun panda-set-python-locals ()
    (setq-local yas-indent-line 'fixed)
    (setq-local yas-also-auto-indent-first-line nil))
  (panda-formatter-def black
    :mode python-mode
    :program "black"
    :required-args ("-" "--quiet")
    :extra-args ("--line-length" "80")
    :config-file "pyproject.toml"))

;;;; R
(use-package ess
  :defer t
  :gfhook ('ess-r-mode-hook '(panda-format-on-save-mode lsp yas-minor-mode))
  :general
  (panda-space-sc :keymaps 'ess-r-mode-map
    "xb" 'ess-eval-buffer
    "xd" 'ess-eval-function
    "xf" 'ess-load-file
    "xe" 'ess-eval-line
    "xp" 'ess-eval-paragraph
    "xr" 'ess-eval-region
    "xx" 'R)
  :init
  (setq ess-ask-for-ess-directory nil
        ess-use-flymake nil)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     '("R" "--slave" "-e" "languageserver::run()"))
                    :major-modes '(ess-r-mode)
                    :server-id 'R)))

;;;; Rust
(use-package rust-mode
  :defer t
  :gfhook '(lsp yas-minor-mode)
  :config
  (panda-formatter-def rustfmt
    :mode rust-mode
    :program "rustfmt"))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;;; Shell Script
(use-package sh-script
  :defer t
  :gfhook ('sh-mode-hook '(panda-format-on-save-mode yas-minor-mode)))

;;;; YAML
(use-package yaml-mode
  :defer t
  :gfhook '(panda-trim-on-save-mode yas-minor-mode))

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (when (fboundp 'outshine-mode) (outshine-mode 1) (outshine-cycle-buffer))
;; End:
