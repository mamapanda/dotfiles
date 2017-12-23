;;; lang-typescript.el --- panda's emacs init typescript file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package typescript-mode
  :defer t)

(use-package tide
  :defer t
  :init
  (defun setup-tide-mode ()
    "Sets up tide-mode."
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  ;(add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
