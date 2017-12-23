;;; lang-common-lisp.el --- panda's emacs init common lisp file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (slime-setup '(slime-fancy)))

(provide 'lang-common-lisp)
;;; lang-common-lisp.el ends here
