;;; base-editing.el --- panda's emacs base editing file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package anzu
  :bind (("C-c r" . anzu-query-replace))
  :init
  (global-anzu-mode t))

(use-package company
  :bind (:map company-active-map
              ("<tab>" . nil) ;make company play nicer with yasnippet
              ("C-w" . nil)) ;kill-line annoyance
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t))

(use-package corral
  :defer t
  :init
  (defhydra hydra-corral (global-map "C-c c")
    ("{" corral-braces-backward)
    ("}" corral-braces-forward)
    ("[" corral-brackets-backward)
    ("]" corral-brackets-forward)
    ("(" corral-parentheses-backward)
    (")" corral-parentheses-forward)
    ("q" nil)))

(use-package expand-region
  :bind (("C-;" . er/expand-region)))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defhydra hydra-flycheck (global-map "C-c f")
    ("p" flycheck-previous-error)
    ("n" flycheck-next-error)
    ("q" nil))
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)))

(use-package multiple-cursors
  :defer t
  :init
  (defhydra hydra-multiple-cursors (global-map "C-c m")
    ("p" mc/mark-previous-like-this)
    ("n" mc/mark-next-like-this)
    ("l" mc/edit-lines)
    ("a" mc/mark-all-like-this :exit t)
    ("q" nil)))

(use-package origami
  :bind (:map origami-mode-map
              ("C-c o o" . origami-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o p" . origami-show-only-node))
  :init
  (global-origami-mode))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-." . undo-tree-redo)
              ("C-?" . nil))
  :init
  (global-undo-tree-mode))

(use-package winner
  :init
  (winner-mode t))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode -1)))
  (setq yas-triggers-in-field t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'yas-indent-line) 'fixed)
              (set (make-local-variable 'yas-also-auto-indent-first-line) 'nil))))

(provide 'base-editing)
;;; base-editing.el ends here
