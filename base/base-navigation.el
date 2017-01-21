;;; base-navigation.el --- panda's emacs base navigation file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "#39FF14"
                      :weight 'bold))

(use-package avy
  :bind (("C-c a" . avy-goto-word-1))
  :config
  (defvar panda/avy-fg-colors '("#39FF14" "#67C8FF" "#FF9933")
    "The foreground colors to use for avy's lead faces.")
  (defun panda/set-avy-faces (fg-colors)
    "Changes avy faces based on current background color & FG-COLORS."
    (let ((avy-face-count (length avy-lead-faces))
          (fg-color-count (length fg-colors)))
      (dotimes (n avy-face-count)
        (set-face-attribute (nth n avy-lead-faces) nil
                            :foreground (nth (mod n fg-color-count) fg-colors)
                            :background (face-attribute 'default :background)
                            :weight 'bold))))
  (panda/set-avy-faces panda/avy-fg-colors)
  (setq avy-background t))

(use-package imenu
  :defer t
  :bind (("C-c i" . imenu))
  :config
  (setq imenu-auto-rescan t))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'alien))

(provide 'base-navigation)
;;; base-navigation.el ends here
