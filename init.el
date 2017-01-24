;;; init.el --- panda's emacs init.el file

;;; Commentary:
;;; bamboo

;;; Code:

;(package-initialize) ;; pls go away

(add-to-list 'load-path (concat user-emacs-directory "base"))
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'base-core)

(require 'base-appearance)
(require 'base-editing)
(require 'base-ido)
(require 'base-navigation)
(require 'base-misc)

(require 'lang-c)
(require 'lang-csharp)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-org)
(require 'lang-rust)
(require 'lang-python)
(require 'lang-typescript)
(require 'lang-web)

(w32-send-sys-command 61488) ;fullscreen / lol if not windows

(provide 'init)
;;; init.el ends here
