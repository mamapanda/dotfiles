;;; lang-rust.el --- panda's emacs init rust file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package rust-mode
  :defer t)

(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
;;; lang-rust.el ends here
