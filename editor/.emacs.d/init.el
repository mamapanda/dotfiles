;;; Init.el Setup  -*- lexical-binding: t -*-
;;;; Startup Optimizations
(defvar panda--pre-init-file-name-handler-alist file-name-handler-alist
  "The value of `file-name-handler-alist' before init.el was loaded.")

(defvar panda--pre-init-gc-cons-threshold gc-cons-threshold
  "The value of `gc-cons-threshold' before init.el was loaded.")

(defun panda--restore-init-optimization-variables ()
  "Restore variables that were modified for init time optimization."
  (setq file-name-handler-alist panda--pre-init-file-name-handler-alist
        gc-cons-threshold       panda--pre-init-gc-cons-threshold))

(setq file-name-handler-alist nil
      gc-cons-threshold       64000000)

(add-hook 'after-init-hook #'panda--restore-init-optimization-variables)

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
(require 'cl-lib)

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
  (setq load-path (cl-nset-difference
                   load-path
                   (mapcar #'straight--build-dir '("goto-chg" "undo-tree"))
                   :test #'file-equal-p))
  :config
  (gsetq evil-move-beyond-eol    t
         evil-toggle-key         "C-s-+"
         evil-want-C-d-scroll    t
         evil-want-C-u-scroll    t
         evil-want-Y-yank-to-eol t)
  (gsetq-default evil-symbol-word-search t)
  (progn
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
  (gsetq evil-collection-key-blacklist '("SPC")
         evil-collection-mode-list (cl-nset-difference
                                    evil-collection-mode-list
                                    '(company outline)))
  (evil-collection-init))

;;; Basic Configuration
;;;; Definitions
;;;;; Defuns
(defun panda-bind-visual-line-motions (keymap)
  "Bind visual line equivalents of evil motions in KEYMAP."
  (general-def 'motion :keymaps keymap
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

(defun panda-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun panda-reload-file ()
  "Reload the current file, preserving point."
  (interactive)
  (if buffer-file-name
      (let ((pos (point)))
        (find-alternate-file buffer-file-name)
        (goto-char pos))
    (message "Buffer is not visiting a file")))

(defun panda-configure-image-view ()
  "Configure settings for viewing an image."
  (display-line-numbers-mode -1)
  (gsetq-local evil-default-cursor (list nil)))

(defun panda-static-evil-ex (&optional initial-input)
  "`evil-ex' that doesn't move point."
  (interactive)
  (save-excursion (call-interactively #'evil-ex)))

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
  "Same as `add-hook', but FN is removed from HOOK after being run."
  (let ((hook-fn-name (gensym)))
    `(progn
       (defun ,hook-fn-name ()
         (funcall ,fn)
         (remove-hook ,hook (quote ,hook-fn-name) ,local))
       (add-hook ,hook (quote ,hook-fn-name) ,append ,local))))

(cl-defmacro panda-with-gui (&body body)
  "Run BODY when a gui is available."
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
  "Set `panda-inner-defun-begin-regexp' and `panda-inner-defun-end-regexp' for MODE.
MODE may be a symbol or a list of modes."
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
       c-default-style '((java-mode . "java")
                         (awk-mode . "awk")
                         (other . "stroustrup"))
       delete-by-moving-to-trash t
       disabled-command-function nil
       enable-recursive-minibuffers t
       inhibit-compacting-font-caches t
       make-backup-files nil
       recentf-max-saved-items 100
       require-final-newline t
       save-abbrevs nil
       tramp-default-method "ssh"
       undo-limit 1000000
       use-dialog-box nil
       vc-follow-symlinks t)

(gsetq-default bidi-display-reordering nil
               buffer-file-coding-system 'utf-8
               c-basic-offset 4
               indent-tabs-mode nil
               tab-width 4
               truncate-lines nil)

(blink-cursor-mode -1)
(delete-selection-mode 1)
(desktop-save-mode)
(electric-pair-mode 1)
(global-auto-revert-mode t)
(recentf-mode 1)
(show-paren-mode 1)

;;;; Keybindings
(general-def '(normal motion) override
  ";"   'panda-static-evil-ex
  ":"   'eval-expression
  ","   'execute-extended-command
  "Q"   'save-buffer)

(general-def 'normal
  "C-r" nil
  "g;"  nil
  "g,"  nil
  "gD"  'xref-find-references
  "[e"  'previous-error
  "]e"  'next-error)

(general-def 'insert "<C-backspace>" 'evil-delete-backward-word)

(general-def 'motion
  "SPC" nil
  ";"   nil
  ","   nil
  "`"   'evil-goto-mark-line
  "'"   'evil-goto-mark
  "gs"  'evil-repeat-find-char
  "gS"  'evil-repeat-find-char-reverse
  "[d"  'panda-backward-defun
  "]d"  'panda-forward-defun)

(general-def 'outer
  "e"  'panda-outer-buffer
  "d"  'panda-outer-defun
  "ld" 'panda-outer-last-defun
  "nd" 'panda-outer-next-defun)

(general-def 'inner
  "e"  'panda-inner-buffer
  "d"  'panda-inner-defun
  "ld" 'panda-inner-last-defun
  "nd" 'panda-inner-next-defun)

(panda-space
  "b" 'switch-to-buffer
  "c" 'compile
  "d" 'dired
  "f" 'find-file
  "h" 'help-command
  "o" 'occur
  "t" 'bookmark-jump
  "T" 'bookmark-set
  "4" '(:keymap ctl-x-4-map)
  "5" '(:keymap ctl-x-5-map)
  "%" (general-key "C-x C-q"))

(evil-ex-define-cmd "bk[ill]" #'panda-kill-this-buffer)

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
  :general (panda-space "l" 'panda-toggle-line-numbers)
  :config
  (progn
    (gsetq display-line-numbers-type 'visual)
    (defun panda-toggle-line-numbers ()
      "Toggle between `display-line-numbers-type' and absolute line numbers.
The changes are local to the current buffer."
      (interactive)
      (gsetq display-line-numbers
             (if (eq display-line-numbers display-line-numbers-type)
                 t
               display-line-numbers-type))))
  (progn
    (defun panda--evil-ex-relative-lines (old-fn &optional initial-input)
      "Enable relative line numbers for `evil-ex'."
      (let ((current-display-line-numbers display-line-numbers))
        (unwind-protect
            (progn
              (gsetq display-line-numbers 'relative)
              (funcall old-fn initial-input))
          (gsetq display-line-numbers current-display-line-numbers))))
    (advice-add 'evil-ex :around #'panda--evil-ex-relative-lines))
  (progn
    (panda-with-gui
      (global-display-line-numbers-mode 1))
    (column-number-mode 1)))

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
  :ghook 'prog-mode-hook)

;;;; Editing
(use-package evil-args
  :general
  ('inner "a" 'evil-inner-arg)
  ('outer "a" 'evil-outer-arg))

(use-package evil-commentary
  :general
  ('normal "gc" 'evil-commentary
           "gy" 'evil-commentary-yank))

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
  :general
  ('normal "gl" 'evil-lion-left
           "gL" 'evil-lion-right))

(use-package evil-numbers
  :general
  ('normal "C-a" 'evil-numbers/inc-at-pt
           "C-s" 'evil-numbers/dec-at-pt))

(use-package evil-replace-with-register
  :general ('normal "gR" 'evil-replace-with-register))

(use-package evil-surround
  :config
  (general-def 'visual evil-surround-mode-map
    "s"  'evil-surround-region
    "S"  'evil-Surround-region
    "gS" nil)
  (global-evil-surround-mode 1))

(use-package evil-traces
  :straight (evil-traces
             :host github
             :repo "mamapanda/evil-traces"
             :local-repo "~/code/emacs-lisp/evil-traces")
  :config
  (evil-traces-use-diff-faces)
  (evil-traces-mode))

(use-package expand-region
  :general ('visual "v" 'er/expand-region))

(use-package targets
  :straight (:type git :host github :repo "noctuid/targets.el")
  :config
  (progn
    (defun panda-show-reg-targets-fix (orig-fn)
      "Advice to not error with `targets--reset-position'."
      (let ((register-alist (cl-remove 'targets--reset-position
                                       register-alist
                                       :key #'car)))
        (funcall orig-fn)))
    (advice-add #'evil-show-registers :around #'panda-show-reg-targets-fix))
  (targets-setup t))

(use-package undo-propose
  :general ('normal "U" 'undo-propose))

;;;; Help
(use-package helpful
  :general
  (help-map "f" 'helpful-callable
            "k" 'helpful-key
            "v" 'helpful-variable))

(use-package which-key
  :disabled t
  :config
  (gsetq which-key-popup-type 'side-window
         which-key-side-window-location 'bottom
         which-key-idle-delay 1.0))

;;;; Keys
(use-package keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(use-package no-spam
  :straight (no-spam
             :fetcher github
             :repo "mamapanda/no-spam"
             :local-repo "~/code/emacs-lisp/no-spam")
  :config
  (gsetq no-spam-default-exception #'evil-operator-state-p)
  (no-spam-add-repeat-delay (evil-forward-char evil-backward-char))
  (no-spam-add-repeat-delay (evil-next-line evil-previous-line))
  (no-spam-add-repeat-delay (evil-forward-WORD-begin evil-backward-WORD-begin))
  (no-spam-add-repeat-delay (evil-forward-word-begin evil-backward-word-begin))
  (no-spam-add-repeat-delay (evil-forward-WORD-end evil-backward-WORD-end))
  (no-spam-add-repeat-delay (evil-forward-word-end evil-backward-word-end))
  (no-spam-add-repeat-delay (evil-forward-paragraph evil-backward-paragraph))
  (no-spam-mode))

;;;; Navigation
(use-package avy
  :general ('motion "C-SPC" 'evil-avy-goto-char-timer)
  :config
  (gsetq avy-all-windows         nil
         avy-all-windows-alt     t
         avy-background          t
         avy-indent-line-overlay t))

(use-package deadgrep
  :general (panda-space "s" 'deadgrep))

(use-package evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :config
  (gsetq evil-snipe-repeat-keys   t
         evil-snipe-smart-case    t
         evil-snipe-scope         'visible
         evil-snipe-repeat-scope  'visible
         evil-snipe-tab-increment t)
  (general-def 'visual evil-snipe-local-mode-map
    "x" 'evil-snipe-x
    "X" 'evil-snipe-X
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S)
  (general-def 'motion evil-snipe-override-local-mode-map
    ";"  nil
    ","  nil
    "gs" 'evil-snipe-repeat
    "gS" 'evil-snipe-repeat-reverse)
  (setf (cdr evil-snipe-parent-transient-map) nil)
  (general-def evil-snipe-parent-transient-map
    "s" 'evil-snipe-repeat
    "S" 'evil-snipe-repeat-reverse)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-visualstar
  :config
  (gsetq evil-visualstar/persistent t)
  (global-evil-visualstar-mode 1))

(use-package imenu
  :general (panda-space "i" 'imenu)
  :config
  (gsetq imenu-auto-rescan t))

(use-package projectile
  :defer t
  :general (panda-space "p" '(:keymap projectile-command-map))
  :config
  (gsetq projectile-indexing-method 'alien
         projectile-completion-system 'ivy)
  (projectile-mode))

;;;; UI Completion
(use-package flx :defer t)
(use-package smex :defer t)

(use-package ivy
  :demand t
  :config
  (gsetq ivy-wrap t
         ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                 (t . ivy--regex-fuzzy))
         confirm-nonexistent-file-or-buffer t
         ivy-count-format "(%d/%d) ")
  (general-def ivy-minibuffer-map
    "<return>"   'ivy-alt-done
    "C-<return>" 'ivy-immediate-done)
  (ivy-mode 1))

(use-package counsel
  :demand t
  :general
  (panda-space
    "F" 'counsel-recentf
    "S" 'counsel-git-grep)
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

(use-package goto-last-change
  :general ('normal "g;" 'goto-last-change))

(use-package helm
  :defer t
  :config
  (gsetq helm-echo-input-in-header-line        t
         helm-ff-fuzzy-matching                nil
         helm-find-files-ignore-thing-at-point t
         helm-split-window-inside-p            t
         helm-mini-default-sources             '(helm-source-buffers-list
                                                 helm-source-recentf))
  (general-def helm-map "<escape>" 'helm-keyboard-quit))

;;;; Windows
(use-package eyebrowse
  :demand t
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
  (gsetq eyebrowse-close-window-config-prompt t
         eyebrowse-new-workspace t)
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
(use-package image-dired
  :defer t
  :gfhook ('image-dired-display-image-mode-hook 'panda-configure-image-view)
  :general (panda-space "D" 'image-dired))

(use-package dired-filter
  :defer t
  :general ('normal dired-mode-map "gf" '(:keymap dired-filter-map)))

(use-package dired-open
  :general ('normal dired-mode-map "<C-return>" 'dired-open-xdg))

(use-package dired-subtree
  :general
  ('normal dired-mode-map
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
  ('normal dired-mode-map
           "gc" 'dired-ranger-copy
           "gm" 'dired-ranger-move
           "gp" 'dired-ranger-paste))

;;;; Git
(use-package magit
  :general (panda-space "g" 'magit-status)
  :config
  (gsetq magit-auto-revert-mode nil))

(use-package magit-todos
  :after magit
  :config
  (gsetq magit-todos-rg-extra-args '("--hidden" "--glob" "!.git/"))
  (magit-todos-mode))

(use-package evil-magit :after magit)

(use-package git-timemachine
  :general (panda-space "G" 'git-timemachine))

;;;; Music
(use-package emms
  :straight nil ; yay -S emms-git
  :general (panda-space "m" 'emms)
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
  :defer t
  :config
  (gsetq elfeed-feeds (panda-get-private-data 'elfeed-feeds)
         elfeed-search-title-max-width 100
         elfeed-search-filter "@1-month-ago +unread"))

(use-package image-mode
  :straight nil
  :defer t
  :gfhook 'panda-configure-image-view)

(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :gfhook 'visual-line-mode
  :config
  (gsetq nov-text-width most-positive-fixnum))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :gfhook ('pdf-view-mode-hook 'panda-configure-image-view)
  :config
  (gsetq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install))

;;;; Shell
(use-package eshell
  :general
  (panda-space "<return>" 'eshell)
  (ctl-x-4-map "<return>" 'panda-eshell-other-window)
  :config
  (gsetq eshell-hist-ignoredups t
         eshell-history-size 1024)
  (defun panda-eshell-other-window (&optional arg)
    "Open `eshell' in another window."
    (interactive "P")
    (switch-to-buffer-other-window
     (save-window-excursion (eshell arg)))))

(use-package esh-autosuggest
  :ghook 'eshell-mode-hook)

(use-package fish-completion
  :ghook 'eshell-mode-hook)

(use-package vterm
  :general
  (panda-space "<S-return>" 'vterm)
  (ctl-x-4-map "<S-return>" 'vterm-other-window)
  :init
  (defvar vterm-install t)
  :config
  (gsetq vterm-shell "fish"))

;;;; System
(use-package disk-usage :defer t)
(use-package helm-system-packages :defer t)

;;; Mode-Specific Configuration
;;;; Completion / Linting
(use-package company
  :defer t
  :init
  (defvar company-active-map (make-sparse-keymap))
  :config
  (gsetq company-backends                  (delete 'company-dabbrev company-backends)
         company-dabbrev-code-modes        nil
         company-minimum-prefix-length     2
         company-tooltip-align-annotations t)
  (general-def company-active-map
    "C-p"      'company-select-previous
    "C-n"      'company-select-next
    "C-b"      'company-previous-page
    "C-f"      'company-next-page
    "<return>" 'company-complete-selection
    "C-g"      'company-abort))

(use-package flycheck
  :defer t
  :config
  (gsetq flycheck-display-errors-delay 0.5)
  (general-def 'normal flycheck-mode-map
    "[e" 'flycheck-previous-error
    "]e" 'flycheck-next-error))

(use-package flycheck-posframe
  :ghook 'flycheck-mode-hook
  :config
  (flycheck-posframe-configure-pretty-defaults))

;;;; Formatting
(use-package reformatter)

;;;; Language Server
(use-package lsp-mode
  :defer t
  :commands lsp-register-client
  :config
  (gsetq lsp-enable-indentation nil
         lsp-enable-on-type-formatting nil
         lsp-prefer-flymake nil)
  ;; LSP does hook onto xref, but these functions are more reliable.
  (general-def 'normal lsp-mode-map
    "gd" 'lsp-find-definition
    "gD" 'lsp-find-references)
  (panda-space-sc lsp-mode-map
    "r"  'lsp-rename
    "\\" 'lsp-restart-workspace))

(use-package company-lsp :after company lsp)

(use-package lsp-ui
  :after lsp
  :config
  (gsetq lsp-ui-sideline-show-diagnostics nil)
  (panda-space-sc lsp-ui-mode-map
    "i" 'lsp-ui-imenu
    "R" 'lsp-ui-sideline-apply-code-actions))

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
         dap--breakpoints-file (no-littering-expand-var-file-name "dap/breakpoints"))
  (dap-mode 1)
  (dap-ui-mode 1))

;;;; Lisp
(use-package lispyville
  :defer t
  :config
  (lispyville-set-key-theme '(additional-motions
                              c-w
                              commentary
                              operators
                              prettify
                              slurp/barf-cp))
  (general-unbind 'motion lispyville-mode-map "{" "}")
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

(use-package lispy
  :disabled t
  :ghook 'lispyville-mode-hook
  :config
  (gsetq lispy-key-theme '(lispy special))
  (lispy-define-key lispy-mode-map-special "<" 'lispy-slurp-or-barf-left)
  (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp-or-barf-right)
  (general-def lispy-mode-map-lispy "\"" 'lispy-doublequote))

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

;;;; View / Layout
(use-package olivetti :defer t)

(use-package outshine
  :defer t
  :config
  (gsetq outshine-org-style-global-cycling-at-bob-p t)
  (general-def 'normal outshine-mode-map
    "<tab>"     (lookup-key outshine-mode-map (kbd "TAB"))
    "<backtab>" 'outshine-cycle-buffer))

;;; Language Modes
;;;; Assembly
(use-package asm-mode
  :defer t
  :gfhook '(asmfmt-on-save-mode panda-set-asm-locals yas-minor-mode)
  :config
  (gsetq asm-comment-char ?#)
  (defun panda-set-asm-locals ()
    (gsetq-local indent-tabs-mode t)
    (gsetq-local tab-always-indent (default-value 'tab-always-indent)))
  (progn
    (defvar asmfmt-args nil
      "Arguments for asmfmt.")
    (reformatter-define asmfmt
      :program "asmfmt"
      :args asmfmt-args)))

;;;; C / C++
(use-package cc-mode
  :defer t
  :gfhook ('(c-mode-hook c++-mode-hook)
           '(clang-format-on-save-mode panda-set-c-locals yas-minor-mode))
  :config
  (defun panda-set-c-locals ()
    (c-set-offset 'innamespace 0))
  (progn
    (defvar clang-format-args nil
      "Arguments for clang-format.")
    (reformatter-define clang-format
      :program "clang-format"
      :args clang-format-args)))

(use-package ccls
  :ghook ('(c-mode-hook c++-mode-hook) 'panda-ccls)
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
  :defer t
  :config
  (gsetq inferior-lisp-program "sbcl"
         slime-contribs '(slime-fancy))
  (panda-space-sc slime-mode-map
    "m"  'macrostep-expand
    ",b" 'slime-eval-buffer
    ",d" 'slime-eval-defun
    ",e" 'slime-eval-last-expression
    ",r" 'slime-eval-region
    ",," 'slime)
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
  :gfhook '(dfmt-on-save-mode lsp yas-minor-mode)
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("dls"))
                      :major-modes '(d-mode)
                      :server-id 'dls))
    (add-to-list 'lsp-language-id-configuration '(d-mode . "d")))
  (progn
    (defvar dfmt-args '("--brace_style=otbs"
                        "--space_after_cast=false"
                        "--max_line_length=80")
      "Arguments for dfmt.")
    (reformatter-define dfmt
      :program "dfmt"
      :args dfmt-args)))

;;;; Emacs Lisp
(use-package elisp-mode
  :straight nil
  :defer t
  :gfhook ('emacs-lisp-mode-hook '(company-mode
                                   lispyville-mode
                                   panda-format-on-save-mode
                                   panda-set-elisp-locals
                                   yas-minor-mode))
  :config
  (panda-space-sc (emacs-lisp-mode-map lisp-interaction-mode-map)
    ",b" 'eval-buffer
    ",d" 'eval-defun
    ",e" 'eval-last-sexp
    ",r" 'eval-region
    ",," 'ielm)
  (defun panda-set-elisp-locals ()
    (gsetq-local evil-args-delimiters '(" "))))

(use-package macrostep
  :general
  (panda-space-sc (emacs-lisp-mode-map lisp-interaction-mode-map)
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
  :gfhook '(gofmt-on-save-mode lsp panda-set-go-locals yas-minor-mode)
  :config
  (defun panda-set-go-locals ()
    (gsetq-local indent-tabs-mode t))
  (progn
    (defvar gofmt-args nil
      "Arguments for gofmt.")
    (reformatter-define gofmt
      :program "gofmt"
      :args gofmt-args)))

;;;; HTML / CSS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :gfhook '(lsp prettier-html-on-save-mode)
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
  (defvar prettier-html-args '("--stdin" "--parser" "html")
    "Arguments for prettier with HTML.")
  (reformatter-define prettier-html
    :program "prettier"
    :args prettier-html-args))

(use-package css-mode
  :defer t
  :gfhook '(lsp prettier-css-on-save-mode)
  :config
  (defvar prettier-css-args '("--stdin" "--parser" "css" "--tab-width" "4")
    "Arguments for prettier with CSS.")
  (reformatter-define prettier-css
    :program "prettier"
    :args prettier-css-args))

(use-package emmet-mode
  :ghook '(web-mode-hook css-mode-hook))

;;;; JavaScript / TypeScript
(use-package js
  :defer t
  :gfhook '(lsp prettier-ts-on-save-mode yas-minor-mode))

(use-package rjsx-mode :defer t)

(use-package typescript-mode
  :defer t
  :gfhook '(lsp prettier-ts-on-save-mode yas-minor-mode))

(defvar prettier-ts-args '("--stdin" "--parser" "typescript" "--tab-width" "4")
  "Arguments for prettier with TypeScript.")

(reformatter-define prettier-ts
  :program "prettier"
  :args prettier-ts-args)

(use-package nodejs-repl
  :general
  (panda-space-sc js-mode-map
    ",b" 'nodejs-repl-send-buffer
    ",e" 'nodejs-repl-send-line
    ",f" 'nodejs-repl-load-file
    ",r" 'nodejs-repl-send-region
    ",," 'nodejs-repl))

;;;; JSON
(use-package json-mode
  :defer t
  :gfhook '(panda-disable-js-hooks prettier-json-on-save-mode)
  :config
  (defun panda-disable-js-hooks () ;; TODO: We probably don't need this anymore with LSP
    (company-mode -1)
    (flycheck-mode -1)
    (prettier-ts-on-save-mode -1))
  (progn
    (defvar prettier-json-args '("--stdin" "--parser" "--json" "--tab-width" "4")
      "Arguments for prettier with JSON.")
    (reformatter-define prettier-json
      :program "prettier"
      :args prettier-json-args)))

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
  :gfhook ('makefile-mode-hook '(panda-trim-on-save-mode yas-minor-mode)))

;;;; Markdown
(use-package markdown-mode
  :defer t
  :gfhook '(prettier-md-on-save-mode yas-minor-mode)
  :config
  (defvar prettier-md-args '("--stdin" "--parser" "markdown")
    "Arguments for prettier with Markdown.")
  (reformatter-define prettier-md
    :program "prettier"
    :args prettier-md-args))

;;;; Org
(use-package org
  :straight nil
  :gfhook 'panda-format-on-save-mode
  :general
  (panda-space
    "a" 'org-agenda
    "A" 'org-capture)
  :config
  (gsetq org-directory "~/org")
  (gsetq org-agenda-custom-commands
         '(("n" "Agenda and unscheduled TODOs"
            ((agenda "")
             (alltodo "" ((org-agenda-overriding-header "Unscheduled TODOs:")
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'timestamp)))))))
         org-agenda-files (list (expand-file-name "agenda" org-directory))
         org-capture-templates '(("d" "Deadline TODO" entry (file "agenda/refile.org")
                                  "* TODO %?\n  DEADLINE: %t")
                                 ("s" "Scheduled TODO" entry (file "agenda/refile.org")
                                  "* TODO %?\n  SCHEDULED: %t")
                                 ("t" "TODO" entry (file "agenda/refile.org")
                                  "* TODO %?"))
         org-catch-invisible-edits 'error
         org-src-fontify-natively t
         org-src-tab-acts-natively t))

(use-package toc-org
  :ghook 'org-mode-hook)

(use-package org-bullets
  :ghook 'org-mode-hook)

(use-package helm-org-rifle :defer t)

(use-package evil-org
  :after org ; :after takes precedence over :demand
  :demand t ; required for evil-org-agenda to work properly
  :ghook 'org-mode-hook
  :config
  (evil-org-set-key-theme '(additional calendar insert navigation))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;; Python
(use-package python
  :defer t
  :gfhook '(black-on-save-mode lsp panda-set-python-locals yas-minor-mode)
  :config
  (gsetq python-indent-offset 4)
  (panda-space-sc python-mode-map
    ",b" 'python-shell-send-buffer
    ",d" 'python-shell-send-defun
    ",f" 'python-shell-send-file
    ",r" 'python-shell-send-region
    ",," 'run-python)
  (defun panda-set-python-locals ()
    (gsetq-local yas-indent-line 'fixed)
    (gsetq-local yas-also-auto-indent-first-line nil))
  (progn
    (defvar black-args '("-" "--quiet" "--line-length" "80")
      "Arguments for black.")
    (reformatter-define black
      :program "black"
      :args black-args)))

;;;; R
(use-package ess
  :defer t
  :gfhook ('ess-r-mode-hook '(panda-format-on-save-mode lsp yas-minor-mode))
  :config
  (gsetq ess-ask-for-ess-directory nil
         ess-use-flymake nil)
  (panda-space-sc ess-r-mode-map
    ",b" 'ess-eval-buffer
    ",d" 'ess-eval-function
    ",f" 'ess-load-file
    ",e" 'ess-eval-line
    ",p" 'ess-eval-paragraph
    ",r" 'ess-eval-region
    ",," 'R)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     '("R" "--slave" "-e" "languageserver::run()"))
                    :major-modes '(ess-r-mode)
                    :server-id 'R))
  (add-to-list 'lsp-language-id-configuration '(ess-r-mode . "r")))

;;;; Rust
(use-package rust-mode
  :defer t
  :gfhook '(lsp rustfmt-on-save-mode yas-minor-mode)
  :config
  (defvar rustfmt-args nil
    "Arguments for rustfmt.")
  (reformatter-define rustfmt
    :program "rustfmt"
    :args rustfmt-args))

(use-package cargo
  :ghook ('rust-mode-hook 'cargo-minor-mode))

;;;; Vim
(use-package vimrc-mode
  :defer t
  :gfhook '(panda-trim-on-save-mode))

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
