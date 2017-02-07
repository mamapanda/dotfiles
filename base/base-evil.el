;;; base-evil.el --- panda's emacs base evil file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package evil
  :defer t)

(use-package evil-leader
  :defer t)

(use-package evil-tutor
  :defer t)

(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(setq evil-toggle-key "")

(require 'evil)

(defalias 'evil-insert-state 'evil-emacs-state)
(setq evil-default-state 'emacs
      evil-emacs-state-cursor '(box "white")
      evil-normal-state-cursor '(box "magenta")
      evil-visual-state-cursor '(box "purple"))
(evil-mode t)

(evil-leader/set-key
  "<SPC>" 'avy-goto-word-1
  "c" 'hydra-corral/body
  "f" (defhydra hydra-evil-flycheck ()
        ("k" flycheck-previous-error)
        ("j" flycheck-next-error)
        ("q" nil)))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jj"
                evil-escape-delay 0.5))

(provide 'base-evil)
;;; base-evil.el ends here
