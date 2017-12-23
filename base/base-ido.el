;;; base-ido.el --- panda's emacs base ido file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq flx-ido-use-faces nil))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode t))

(use-package smex ;slight lag if deferred
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

(provide 'base-ido)
;;; base-ido.el ends here
