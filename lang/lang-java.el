;;; lang-java.el --- panda's emacs init java file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ensime ;;installed through panda/packages bc pinned
  :defer t
  :init
  (defun panda/maven-ensime()
    "Activates ensime only if .ensime file is present."
    (when (locate-dominating-file default-directory ".ensime")
      (ensime)))
  (add-hook 'java-mode-hook 'panda/maven-ensime)
  (add-hook 'scala-mode-hook 'panda/maven-ensime)
  :config
  (setq ensime-completion-style nil)
  (add-to-list 'company-backends 'ensime-company))

(provide 'lang-java)
;;; lang-java.el ends here
