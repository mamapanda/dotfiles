;;; lang-org.el --- panda's emacs init org file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package org
  :defer t
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t))

(add-hook 'org-mode-hook (lambda () (yas-minor-mode -1)))

(provide 'lang-org)
;;; lang-org.el ends here
