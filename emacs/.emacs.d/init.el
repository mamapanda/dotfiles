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
(setq package-enable-at-startup nil
      straight-check-for-modifications '(check-on-save find-when-checking))

;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

;;;; Libraries
(use-package general
  :config
  (defalias 'gsetq 'general-setq)
  (defalias 'gsetq-default 'general-setq-default)
  (defalias 'gsetq-local 'general-setq-local))

(use-package no-littering)

;;;; Custom File
(gsetq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Private Data
(defvar panda-private-file (no-littering-expand-etc-file-name "private.el")
  "File for private/sensitive configuration values.
It should contain an alist literal for `panda-get-private-data'.")

(defun panda-get-private-data (key)
  "Get the private configuration value corresponding to KEY."
  (let ((data (with-temp-buffer
                (insert-file-contents panda-private-file)
                (read (buffer-string)))))
    (alist-get key data)))

;;; Evil
(use-package evil
  :init
  (gsetq evil-want-keybinding nil)
  :config
  (gsetq evil-move-beyond-eol    t
         evil-toggle-key         "C-s-+"
         evil-want-C-d-scroll    t
         evil-want-C-u-scroll    t
         evil-want-Y-yank-to-eol t)
  (gsetq-default evil-symbol-word-search t)
  (progn
    (general-evil-setup)
    ;; for bindings that will stay constant
    (general-create-definer panda-space
      :states '(normal operator motion visual)
      :keymaps 'override
      :prefix "SPC"
      :prefix-map 'panda-space-map)
    ;; for bindings that may change depending on active minor modes
    (general-create-definer panda-space-sc
      :states '(normal operator motion visual)
      :prefix "SPC ;"))
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (evil-mode 1))

(use-package evil-collection
  :config
  (gsetq evil-collection-key-blacklist '("SPC"))
  (delete 'company evil-collection-mode-list)
  (evil-collection-init))

;;; Basic Configuration
;;;; Definitions
;;;;; Defuns
(defun panda-bind-visual-line-motions (keymap)
  "Bind visual line equivalents of evil motions in KEYMAP."
  (general-mmap :keymaps keymap
    "j"  'evil-next-visual-line
    "k"  'evil-previous-visual-line
    "0"  'evil-beginning-of-visual-line
    "^"  'evil-first-non-blank-of-visual-line
    "$"  'evil-end-of-visual-line
    "gj" 'evil-next-line
    "gk" 'evil-previous-line
    "g0" 'evil-beginning-of-line
    "g^" 'evil-first-non-blank
    "g$" 'evil-end-of-line))

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

(defun panda-sudo-reload-file ()
  "Reload the current file with root privileges, preserving point."
  (interactive)
  (if buffer-file-name
      (let ((pos (point)))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
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

(cl-defmacro panda-disable-repeat (command)
  "Disable repeating COMMAND."
  (cl-assert (commandp command))
  (let ((advice-name (intern (format "panda--no-repeat-%s" command))))
    `(progn
       (defun ,advice-name (old-fn &rest args)
         ,(format "Advice to disable repeating %s." command)
         (when (or (not (eq last-command (function ,command)))
                   (not (called-interactively-p))
                   (evil-operator-state-p))
           (apply old-fn args))
         ;; hopefully this doesn't blow up
         (setq this-command (function ,command)))
       (advice-add (quote ,command) :around (function ,advice-name)))))

(cl-defmacro panda-with-gui (&body body)
  "If a daemon is running, then add BODY to `after-make-frame-functions'
with a lambda wrapper. Else, simply evaluate BODY."
  (declare (indent defun))
  (if (daemonp)
      `(add-to-list 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame frame
                        ,@body)))
    `(progn ,@body)))

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
                         (gsetq-local panda-inner-defun-begin-regexp ,begin)
                         (gsetq-local panda-inner-defun-end-regexp ,end)))))

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

;;;; Appearance
(gsetq default-frame-alist '((fullscreen . maximized)
                             (font . "Consolas-11")
                             (menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (vertical-scroll-bars . nil))
       inhibit-startup-screen t
       ring-bell-function 'ignore
       visible-bell nil)

;;;; Behavior
(gsetq auto-save-default nil
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
       tramp-default-method "ssh"
       vc-follow-symlinks t)

(gsetq-default bidi-display-reordering nil
               buffer-file-coding-system 'utf-8
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

(panda-disable-repeat evil-forward-char)
(panda-disable-repeat evil-next-line)
(panda-disable-repeat evil-previous-line)
(panda-disable-repeat evil-backward-char)

;;;; Keybindings
(general-nmap :keymaps 'override
  "Q" 'save-buffer)

(general-nmap
  "C-r"        nil
  "U"          'redo
  "_"          'eval-expression
  "[ SPC"      'panda-insert-space-before
  "] SPC"      'panda-insert-space-after
  "[ <return>" 'panda-insert-newline-before
  "] <return>" 'panda-insert-newline-after)

(general-imap "<C-backspace>" 'evil-delete-backward-word)

(general-mmap
  "SPC" nil
  "[d"  'panda-backward-defun
  "]d"  'panda-forward-defun)

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

(panda-space
  "SPC" 'execute-extended-command
  "b"   'switch-to-buffer
  "B"   'kill-buffer
  "d"   'dired
  "f"   'find-file
  "h"   'help-command
  "t"   'bookmark-jump
  "T"   'bookmark-set
  "4"   '(:keymap ctl-x-4-map)
  "%"   (general-key "C-x C-q"))

(panda-space-sc
  "c" 'compile
  "k" 'previous-error
  "j" 'next-error
  "x" 'xref-find-references)

;;; Global Packages
;;;; Appearance
(use-package base16-theme
  :defer t
  :config
  (gsetq base16-distinct-fringe-background nil)
  (panda-with-gui
    (load-theme 'base16-oceanicnext t)))

(use-package doom-themes
  :config
  (panda-with-gui (load-theme 'doom-vibrant t)))

(use-package display-line-numbers
  :demand t
  :general
  (panda-space "l" 'panda-toggle-line-numbers)
  :config
  (gsetq-default display-line-numbers-type 'relative)
  (defun panda-toggle-line-numbers ()
    "Toggle between relative and absolute line numbers in current buffer."
    (interactive)
    (gsetq-local display-line-numbers-type (cl-case display-line-numbers-type
                                             (relative t)
                                             ((t) 'relative)
                                             (otherwise 'relative)))
    (display-line-numbers-mode 1))
  (panda-with-gui
    (global-display-line-numbers-mode 1))
  (column-number-mode 1))

(use-package doom-modeline
  :config
  (gsetq doom-modeline-buffer-file-name-style 'relative-from-project
         doom-modeline-icon nil)
  (panda-with-gui
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
  (gsetq evil-goggles-pulse nil)
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
  :straight nil
  :general
  (panda-space "o" 'occur)
  (general-nmap occur-mode-map
    "C-<return>" 'occur-mode-display-occurrence))

(use-package targets
  :straight (:type git :host github :repo "noctuid/targets.el")
  :config
  (targets-setup t))

(use-package undo-tree
  :defer t
  :config
  (gsetq undo-tree-enable-undo-in-region nil))

;;;; Help
(use-package which-key
  :config
  (gsetq which-key-popup-type 'side-window
         which-key-side-window-location 'bottom
         which-key-idle-delay 1.0)
  (which-key-mode 1))

;;;; Navigation
(use-package avy
  :general
  (general-mmap "C-SPC" 'evil-avy-goto-char-timer)
  :config
  (gsetq avy-all-windows         nil
         avy-all-windows-alt     t
         avy-background          t
         avy-indent-line-overlay t))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :config
  (gsetq evil-snipe-repeat-keys  nil
         evil-snipe-smart-case   t
         evil-snipe-scope        'visible
         evil-snipe-repeat-scope 'visible)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-visualstar
  :config
  (gsetq evil-visualstar/persistent t)
  (global-evil-visualstar-mode 1))

(use-package imenu
  :general
  (panda-space-sc "i" 'imenu)
  :config
  (gsetq imenu-auto-rescan t))

(use-package projectile
  :defer t
  :general
  (panda-space "p" '(:keymap projectile-command-map))
  :config
  (gsetq projectile-indexing-method 'alien
         projectile-completion-system 'ivy)
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
  :config
  (gsetq ivy-wrap t
         ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                 (t . ivy--regex-fuzzy))
         confirm-nonexistent-file-or-buffer t
         ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package counsel
  :demand t
  :general
  (panda-space
    "F" 'counsel-recentf
    "s" 'counsel-git-grep
    "S" 'counsel-rg)
  :config
  (counsel-mode 1))

(use-package ivy-hydra :defer t)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer-other-window
                               'ivy-rich--ivy-switch-buffer-transformer)
  (with-eval-after-load 'counsel-projectile
    (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer
                                 'ivy-rich--ivy-switch-buffer-transformer))
  (ivy-rich-mode 1))

(use-package counsel-projectile
  :after counsel projectile
  :config
  (counsel-projectile-mode 1))

(use-package helm
  :general
  (general-def :keymaps 'helm-map
    "<escape>" 'helm-keyboard-quit)
  :config
  (gsetq helm-echo-input-in-header-line        t
         helm-ff-fuzzy-matching                nil
         helm-find-files-ignore-thing-at-point t
         helm-split-window-inside-p            t
         helm-mini-default-sources             '(helm-source-buffers-list
                                                 helm-source-recentf)))

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
;;;; File Manager
(use-package dired-filter
  :defer t
  :general
  (panda-space-sc dired-mode-map
    "f" '(:keymap dired-filter-map)))

(use-package dired-open
  :general
  (general-nmap :keymaps 'dired-mode-map
    "<C-return>" 'dired-open-xdg))

(use-package dired-subtree
  :general
  (general-nmap dired-mode-map
    "zo"    'panda-dired-subtree-insert
    "zc"    'panda-dired-subtree-remove
    "za"    'dired-subtree-toggle
    "<tab>" 'dired-subtree-cycle)
  :config
  (defun panda-dired-subtree-insert ()
    "Like `dired-subtree-insert', but doesn't move point."
    (interactive)
    (save-excursion
      (dired-subtree-insert)))
  (defun panda-dired-subtree-remove ()
    "Like `dired-subtree-remove', but removes the current node's children."
    (interactive)
    (when (dired-subtree--is-expanded-p)
      (dired-next-line 1)
      (dired-subtree-remove))))

(use-package dired-ranger
  :general
  (panda-space-sc dired-mode-map
    "c" 'dired-ranger-copy
    "m" 'dired-ranger-move
    "p" 'dired-ranger-paste))

;;;; Git
(use-package magit
  :general
  (panda-space "g" 'magit-status)
  :config
  (gsetq magit-auto-revert-mode nil))

(use-package magit-todos
  :after magit
  :config
  (gsetq magit-todos-rg-extra-args '("--hidden" "--glob" "!.git/"))
  (magit-todos-mode))

(use-package evil-magit :after magit)

(use-package git-timemachine
  :general
  (panda-space "G" 'git-timemachine))

;;;; Music
(use-package emms
  :straight nil ; yay -S emms-git
  :general
  (panda-space "m" 'emms)
  :config
  (require 'emms-setup)
  (require 'emms-info-libtag)
  (emms-all)
  (defun panda-emms-track-description (track)
    "Return a description of TRACK.
This is adapted from `emms-info-track-description'."
    (let ((artist (emms-track-get track 'info-artist))
          (title  (emms-track-get track 'info-title)))
      (cond ((and artist title) (concat title " - " artist))
            (title title)
            (t (emms-track-simple-description track)))))
  (gsetq emms-info-functions                      '(emms-info-libtag)
         emms-player-list                         '(emms-player-vlc)
         emms-repeat-playlist                     t
         emms-source-file-default-directory       "~/Music"
         emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
         emms-track-description-function          'panda-emms-track-description))

;;;; Readers
(use-package elfeed
  :config
  (gsetq elfeed-feeds (panda-get-private-data 'elfeed-feeds)
         elfeed-search-title-max-width 100
         elfeed-search-filter "@1-month-ago +unread"))

(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :gfhook '(visual-line-mode)
  :config
  (gsetq nov-text-width most-positive-fixnum))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :gfhook ('pdf-view-mode-hook 'panda-set-pdf-locals)
  :config
  (gsetq-default pdf-view-display-size 'fit-page)
  (defun panda-set-pdf-locals ()
    (display-line-numbers-mode -1)
    (gsetq-local evil-default-cursor (list nil)))
  (pdf-tools-install))

;;;; Shell
(use-package eshell
  :general
  (panda-space "<return>" 'eshell)
  (general-def ctl-x-4-map "<return>" 'panda-eshell-other-window)
  :config
  (defun panda-eshell-other-window (&optional arg)
    "Open `eshell' in another window."
    (interactive "P")
    (switch-to-buffer-other-window
     (save-window-excursion (eshell arg)))))

;;;; System
(use-package disk-usage :defer t)
(use-package helm-system-packages :defer t)

;;; Mode-Specific Configuration
;;;; Completion / Linting
(use-package company
  :defer t
  :general
  (general-def company-active-map
    "C-p"      'company-select-previous
    "C-n"      'company-select-next
    "C-b"      'company-previous-page
    "C-f"      'company-next-page
    "<return>" 'company-complete-selection
    "C-g"      'company-abort)
  :init
  (defvar company-active-map (make-sparse-keymap))
  :config
  (gsetq company-dabbrev-code-modes nil
         company-minimum-prefix-length 2
         company-tooltip-align-annotations t)
  (delete 'company-dabbrev company-backends))

(use-package flycheck
  :defer t
  :general
  (panda-space-sc flycheck-mode-map
    "e" 'flycheck-list-errors
    "j" 'flycheck-next-error
    "k" 'flycheck-previous-error)
  :config
  (gsetq flycheck-display-errors-delay 0.5))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

;;;; Formatting
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
               (gsetq-local ,args-name (quote ,required-args)))))
       ,@(mapcar (lambda (mode)
                   (let ((mode-hook (intern (format "%s-hook" mode))))
                     `(add-hook ',mode-hook #',setup-fn-name)))
                 mode-list))))

;;;; Language Server
(use-package lsp-mode
  :commands lsp-register-client
  :general
  (panda-space-sc lsp-mode-map
    "r"  'lsp-rename
    "x"  'lsp-find-references
    "\\" 'lsp-restart-workspace)
  :config
  (gsetq lsp-enable-indentation nil
         lsp-enable-on-type-formatting nil
         lsp-prefer-flymake nil))

(use-package company-lsp :after company lsp)

(use-package lsp-ui
  :after lsp
  :general
  (panda-space-sc lsp-ui-mode-map
    "i" 'lsp-ui-imenu
    "R" 'lsp-ui-sideline-apply-code-actions)
  :config
  (gsetq lsp-ui-sideline-show-diagnostics nil))

(use-package dap-mode
  :general
  (panda-space-sc lsp-mode-map
    "d" 'dap-debug
    "D" 'dap-hydra)
  :config
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-gdb-lldb)
  (require 'dap-go)
  (require 'dap-python)
  ;; `dap--breakpoints-file' is declared with `defconst'
  (gsetq dap-utils-extension-path (no-littering-expand-var-file-name "dap")
         dap--breakpoints-file) (no-littering-expand-var-file-name "dap/breakpoints")
  (dap-mode 1)
  (dap-ui-mode 1))

;;;; Lisp
(use-package lispyville
  :defer t
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

;;;; Snippets
(use-package yasnippet
  :config
  (gsetq yas-triggers-in-field nil
         yas-indent-line 'auto
         yas-also-auto-indent-first-line t)
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

;;;; View / Layout
(use-package olivetti :defer t)

(use-package outshine
  :defer t
  :general
  (panda-space-sc outshine-mode-map
    "I" 'outshine-imenu)
  :config
  (gsetq outshine-org-style-global-cycling-at-bob-p t)
  ;; this is down here because of the `lookup-key' call
  (general-nmap :keymaps 'outshine-mode-map
    "<tab>"     (lookup-key outshine-mode-map (kbd "TAB"))
    "<backtab>" 'outshine-cycle-buffer)
  ;; needed for `outshine-imenu', since outshine doesn't load imenu
  (require 'imenu))

;;; Language Modes
;;;; Assembly
(use-package asm-mode
  :defer t
  :gfhook '(panda-set-asm-locals yas-minor-mode)
  :config
  (gsetq asm-comment-char ?#)
  (defun panda-set-asm-locals ()
    (gsetq-local indent-tabs-mode t)
    (gsetq-local tab-always-indent (default-value 'tab-always-indent)))
  (panda-formatter-def asmfmt
    :mode asm-mode
    :program "asmfmt"))

;;;; C / C++
(use-package cc-mode
  :defer t
  :gfhook ('(c-mode-hook c++-mode-hook) '(panda-set-c-locals yas-minor-mode))
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
  :straight nil
  :defer t
  :gfhook '(company-mode
            lispyville-mode
            panda-format-on-save-mode
            panda-set-lisp-locals
            yas-minor-mode)
  :config
  (defun panda-set-lisp-locals ()
    (gsetq-local evil-args-delimiters '(" "))))

(use-package slime
  :general
  (panda-space-sc slime-mode-map
    "m"  'macrostep-expand
    ",b" 'slime-eval-buffer
    ",d" 'slime-eval-defun
    ",e" 'slime-eval-last-expression
    ",r" 'slime-eval-region
    ",," 'slime)
  :config
  (gsetq inferior-lisp-program "sbcl"
         slime-contribs '(slime-fancy))
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
  :straight nil
  :defer t
  :gfhook ('emacs-lisp-mode-hook '(company-mode
                                   lispyville-mode
                                   panda-format-on-save-mode
                                   panda-set-elisp-locals
                                   yas-minor-mode))
  :general
  (panda-space-sc '(emacs-lisp-mode-map lisp-interaction-mode-map)
    ",b" 'eval-buffer
    ",d" 'eval-defun
    ",e" 'eval-last-sexp
    ",r" 'eval-region
    ",," 'ielm)
  :config
  (defun panda-set-elisp-locals ()
    (gsetq-local evil-args-delimiters '(" "))))

(use-package macrostep
  :general
  (panda-space-sc '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m" 'macrostep-expand))

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
    (gsetq-local indent-tabs-mode t))
  (panda-formatter-def gofmt
    :mode go-mode
    :program "gofmt"))

;;;; HTML / CSS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :gfhook '(lsp)
  :init
  (gsetq web-mode-enable-auto-closing t
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

(use-package nodejs-repl
  :general
  (panda-space-sc js-mode-map
    ",b" 'nodejs-repl-send-buffer
    ",e" 'nodejs-repl-send-line
    ",f" 'nodejs-repl-load-file
    ",r" 'nodejs-repl-send-region
    ",," 'nodejs-repl))

(use-package tide
  :disabled t
  :hook ((js-mode typescript-mode) . panda-enable-tide)
  :general
  (panda-space-sc tide-mode-map
    "r"  'tide-rename-symbol
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
  :straight auctex
  :defer t
  :gfhook ('LaTeX-mode-hook '(panda-format-on-save-mode yas-minor-mode))
  :config
  (gsetq TeX-auto-save t
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
  :config
  (gsetq org-agenda-files (directory-files-recursively "~/org/agenda" "\\.org$")
         org-agenda-custom-commands
         '(("n" "Agenda and unscheduled TODOs"
            ((agenda "")
             (alltodo ""
                      ((org-agenda-overriding-header "Unscheduled TODOs:")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'timestamp)))))))
         org-src-fontify-natively t
         org-src-tab-acts-natively t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package helm-org-rifle :defer t)

(use-package evil-org
  :after org ; :after takes precedence over :demand
  :demand t ; required for evil-org-agenda to work properly
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
  (panda-space-sc python-mode-map
    ",b" 'python-shell-send-buffer
    ",d" 'python-shell-send-defun
    ",f" 'python-shell-send-file
    ",r" 'python-shell-send-region
    ",," 'run-python)
  :config
  (gsetq python-indent-offset 4)
  (defun panda-set-python-locals ()
    (gsetq-local yas-indent-line 'fixed)
    (gsetq-local yas-also-auto-indent-first-line nil))
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
  (panda-space-sc ess-r-mode-map
    ",b" 'ess-eval-buffer
    ",d" 'ess-eval-function
    ",f" 'ess-load-file
    ",e" 'ess-eval-line
    ",p" 'ess-eval-paragraph
    ",r" 'ess-eval-region
    ",," 'R)
  :config
  (gsetq ess-ask-for-ess-directory nil
         ess-use-flymake nil)
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

;;; Fun
(use-package 2048-game :defer t)

;;; End Init
(provide 'init)

;; Local Variables:
;; eval: (when (fboundp 'outshine-mode) (outshine-mode 1))
;; End:
