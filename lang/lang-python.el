;;; lang-python.el --- panda's emacs init python file

;;; Commentary:
;;; bamboo

;;; Code:
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local yas-indent-line 'fixed)
            (setq-local yas-also-auto-indent-first-line 'nil)))

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(provide 'lang-python)
;;; lang-python.el ends here
