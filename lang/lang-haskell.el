;;; lang-haskell.el --- panda's emacs init haskell file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package haskell-mode
  :defer t)

(use-package intero
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (flycheck-add-next-checker 'intero '(info . haskell-hlint)))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
