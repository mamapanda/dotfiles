;;; lang-haskell.el --- panda's emacs init haskell file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq flycheck-disabled-checkers '(haskell-stack-ghc)))))

(use-package company-ghc
  :after haskell-mode
  :config
  (add-to-list 'company-backends 'company-ghc))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
