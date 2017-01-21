;;; lang-python.el --- panda's emacs init python file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(provide 'lang-python)
;;; lang-python.el ends here
