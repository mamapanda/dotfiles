;;; init.el --- panda's emacs init.el file

;;; Commentary:
;;; bamboo

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize) ;; pls go away

(add-to-list 'load-path (concat user-emacs-directory "base"))
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'base-core.el)

(require 'base-appearance)
(require 'base-editing)
(require 'base-ido)
(require 'base-navigation)
(require 'base-misc)

(require 'lang-c)
(require 'lang-csharp)
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
