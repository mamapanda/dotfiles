;;; lang-go.el --- panda's emacs init go file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package go-mode
  :defer t)

(use-package go-eldoc
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :after go-mode
  :config
  (add-to-list 'company-backends 'company-go))

(provide 'lang-go)
;;; lang-go.el ends here
