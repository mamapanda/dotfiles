;;; lang-markdown.el --- panda's emacs init markdown file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package markdown-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local panda/delete-trailing-whitespace? nil))))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
