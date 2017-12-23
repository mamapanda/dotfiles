;;; lang-javascript.el --- panda's emacs init javascript file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)))

(use-package tern
  :defer t
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
