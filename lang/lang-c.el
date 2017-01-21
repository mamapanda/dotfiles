;;; lang-c.el --- panda's emacs init c file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package irony
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  (defun my-irony-mode-hook ()
    "Replaces completion functions with irony's completion functions."
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

(use-package company-irony
  :after company-irony-c-headers
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package company-irony-c-headers
  :after irony
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(provide 'lang-c)
;;; lang-c.el ends here
