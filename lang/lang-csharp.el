;;; lang-csharp.el --- panda's emacs init c# file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package csharp-mode
  :defer t)

(use-package omnisharp
  :defer t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (setq omnisharp-server-executable-path
        "~/.emacs.d/omnisharp-roslyn/artifacts/publish/OmniSharp/default/net46/omnisharp.exe")
  (add-to-list 'company-backends 'company-omnisharp))

(provide 'lang-csharp)
;;; lang-csharp.el ends here
