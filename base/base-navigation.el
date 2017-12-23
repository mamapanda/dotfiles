;;; base-navigation.el --- panda's emacs base navigation file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground panda/neon-green
                      :weight 'bold))

(use-package avy
  :bind (("C-c SPC" . avy-goto-word-1))
  :config
  (set-face-attribute 'avy-lead-face nil
                      :foreground panda/neon-green
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground panda/light-blue
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'avy-lead-face-2 nil
                      :foreground panda/deep-saffron
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (setq avy-background t))

(use-package imenu
  :defer t
  :bind (("C-c i" . imenu))
  :config
  (setq imenu-auto-rescan t))

(use-package neotree
  :after projectile
  :bind (("C-c t" . panda/neotree-toggle))
  :config
  (defun panda/neotree-toggle ()
    (interactive)
    (if (get-buffer-window " *NeoTree*" 'visible)
        (neotree-hide)
      (if (projectile-project-p)
          (neotree-dir (projectile-project-root))
        (neotree-show))))
  (setq neo-theme 'arrow
        neo-window-width 30
        neo-window-position 'left))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'ivy))

(provide 'base-navigation)
;;; base-navigation.el ends here
