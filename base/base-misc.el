;;; base-misc.el --- panda's emacs base misc file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-auto-revert-mode nil))

(provide 'base-misc)
;;; base-misc.el ends here
