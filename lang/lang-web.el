;;; lang-web.el --- panda's emacs init web file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package web-mode
  :defer t
  :mode (("\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 4))

(provide 'lang-web)
;;; lang-web.el ends here
