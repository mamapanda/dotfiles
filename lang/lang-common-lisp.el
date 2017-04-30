;;; lang-common-lisp.el --- panda's emacs init common lisp file

;;; Commentary:
;;; bamboo

;;; Code:

(setq inferior-lisp-program (executable-find "sbcl"))

(use-package slime
  :defer t
  :config
  (slime-setup '(slime-fancy)))

(provide 'lang-common-lisp)
;;; lang-common-lisp.el ends here
