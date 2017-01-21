;;; base-misc.el --- panda's emacs base misc file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package god-mode
  :bind (("C-c e" . panda/god-mode)
         :map god-local-mode-map
         ("C-x C-b" . ido-switch-buffer)
         ("C-x C-k" . ido-kill-buffer)
         ("C-x C-o" . ace-window)
         ("C-x C-0" . delete-window)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-c C-a" . avy-goto-word-1)
         ("C-c C-e" . panda/god-mode)
         ("C-c C-s" . occur)
         ("C-c C-d" . multi-occur)
         ("C-c C-n" . nlinum-mode)
         ("C-c C-o C-o" . origami-toggle-node)
         ("C-c C-o C-a" . origami-toggle-all-nodes)
         ("C-c C-o C-p" . origami-show-only-node)
         ("C-c C-p C-f" . projectile-find-file)
         ("C-c C-p C-k" . projectile-kill-buffers)
         ("C-c C-p C-p" . projectile-switch-project))
  :init
  (defvar panda/emacs-cursor (face-attribute 'cursor :background)
    "The cursor color to use in regular mode.")
  (defvar panda/god-cursor "magenta"
    "The cursor color to use in god-mode")
  (defun panda/god-mode()
    "Basically god-mode, but with cursor color changing."
    (interactive)
    (god-mode-all)
    (if (bound-and-true-p god-local-mode)
        (set-cursor-color panda/god-cursor)
      (set-cursor-color panda/emacs-cursor))))

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-auto-revert-mode nil))

(provide 'base-misc)
;;; base-misc.el ends here
