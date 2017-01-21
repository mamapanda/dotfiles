;;; base-ido.el --- panda's emacs base ido file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ido
  :init
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode t)
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-ubiquitous
  :init
  (ido-ubiquitous-mode t))

(use-package smex
  :bind (("M-x" . smex))
  :init
  (smex-initialize))

(provide 'base-ido)
;;; base-ido.el ends here
