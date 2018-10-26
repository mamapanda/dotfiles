;;; lang-assembly.el --- panda's emacs init c file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package asm-mode
  :config
  (add-hook 'asm-mode-hook
            (lambda ()
              (setq-local tab-always-indent (default-value 'tab-always-indent))))
  (setq asm-comment-char ?#))

(use-package gas-mode
  :disabled ; looks weird
  :load-path "~/.emacs.d/site-lisp"
  :mode (("\\.s\\'" . gas-mode)
         ("\\.asm$" . gas-mode)))

(provide 'lang-assembly)
;;; lang-assembly.el ends here
