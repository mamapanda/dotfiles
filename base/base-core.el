;;; base-core.el --- panda's emacs base core file

;;; Commentary:
;;; basically anything that doesn't use use-package

;;; Code:

(require 'package)

(setq-default package-archives
              '(("gnu"     . "http://elpa.gnu.org/packages/")
                ("melpa"        . "http://melpa.org/packages/")
                ("melpa-stable" . "http://stable.melpa.org/packages/"))
              package-archive-priorities
              '(("gnu" . 1)
                ("melpa" . 10)
                ("melpa-stable" . 0)))

(defvar panda/packages
  '(clojure-mode
    ensime
    esup
    fireplace
    git-timemachine
    hydra
    monokai-theme
    pacmacs
    spacemacs-theme
    use-package)
  "A list of packages to ensure are installed.")

(setq package-pinned-packages '((ensime . "melpa-stable")))

(setq package-enable-at-startup nil) ;so it doesn't run twice
(package-initialize)

(defun panda/ensure-packages (packages)
  "Ensures all packages in PACKAGES are installed."
  (let ((refreshed? nil))
    (dolist (package packages)
      (unless (package-installed-p package)
        (unless refreshed?
          (package-refresh-contents)
          (setq refreshed? t))
        (package-install package)))))

(panda/ensure-packages panda/packages)

(setq custom-file "~/.emacs.d/custom-file.el") ;separate file for custom.el
(load custom-file 'noerror)

(global-auto-revert-mode t) ;reloads file if changed externally
(set-frame-font "Consolas-10") ;why emacs keep resetting my font
(setq disabled-command-function nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun panda/C-w ()
  "If a region is active, kill it; else, kill the current line."
  (interactive)
  (call-interactively (if (region-active-p)
                          'kill-region
                        'kill-whole-line)))

(global-set-key (kbd "C-w") 'panda/C-w)
(global-set-key (kbd "C-c s") 'occur)
(global-set-key (kbd "C-c d") 'multi-occur)

(require 'use-package)
(setq use-package-always-ensure t)

(load-theme 'monokai t)

(provide 'base-core.el)
;;; base-core.el ends here
