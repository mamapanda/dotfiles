;;; base-appearance.el --- panda's emacs base appearance file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package nlinum
  :bind (("C-c n" . nlinum-mode)))

(use-package nyan-mode
  :init
  (nyan-mode t))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-modified-off)
  (spaceline-toggle-hud-off))

(provide 'base-appearance)
;;; base-appearance.el ends here
