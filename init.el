;;; init.el --- panda's emacs init.el file

;;; Commentary:
;;; bamboo

;;; Code:

;;(package-initialize) ;; pls go away

(add-to-list 'load-path (concat user-emacs-directory "base"))
(add-to-list 'load-path (concat user-emacs-directory "lang"))

(require 'base-core)

; (require 'base-appearance)
; (require 'base-editing)
; (require 'base-ivy)
; (require 'base-navigation)
; (require 'base-misc)

(require 'lang-assembly)
(require 'lang-c)
(require 'lang-common-lisp)
(require 'lang-csharp)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-javascript)
(require 'lang-markdown)
(require 'lang-org)
(require 'lang-rust)
(require 'lang-powershell)
(require 'lang-python)
(require 'lang-typescript)
(require 'lang-web)

(when (string-equal system-type "windows-nt")
  (w32-send-sys-command 61488))

(provide 'init)
;;; init.el ends here
