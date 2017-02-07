;;; base-misc.el --- panda's emacs base misc file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-auto-revert-mode nil))

(use-package which-key
  :disabled t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer))

(provide 'base-misc)
;;; base-misc.el ends here
