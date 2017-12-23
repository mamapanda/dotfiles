;;; base-editing.el --- panda's emacs base editing file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package anzu
  :bind (("C-c r" . anzu-query-replace))
  :config
  (global-anzu-mode t))

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-w" . nil)) ;kill-line annoyance
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (delete 'company-dabbrev company-backends)
  (setq company-dabbrev-code-modes nil
        company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t))

(use-package corral
  :bind (("C-c c" . hydra-corral/body))
  :config
  (defhydra hydra-corral ()
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
  :bind (("C-c f" . hydra-flycheck/body))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defhydra hydra-flycheck ()
    ("p" flycheck-previous-error)
    ("n" flycheck-next-error)
    ("q" nil))
  (setq flycheck-check-syntax-automatically
        '(mode-enabled save idle-change new-line)))

(use-package multiple-cursors
  :bind (("C-c m" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors ()
    ("p" mc/mark-previous-like-this)
    ("n" mc/mark-next-like-this)
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("q" nil)))

(use-package origami
  :demand t
  :bind (:map origami-mode-map
              ("C-c o o" . origami-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o p" . origami-show-only-node))
  :config
  (global-origami-mode))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-." . undo-tree-redo)
              ("C-?" . nil))
  :config
  (global-undo-tree-mode))

(use-package winner
  :config
  (winner-mode t))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode -1)))
  (eval-after-load 'company
    (progn
      (defun company-yasnippet ()
        "Gives priority to yas completion over company completion."
        (interactive)
        (let ((yas-fallback-behavior nil))
          (unless (yas-expand)
            (call-interactively #'company-complete-selection))))
      (defun company-yas-tab ()
        "Substitutes company's key def to allow priority for yas completion."
        (substitute-key-definition 'company-complete-selection
                                   'company-yasnippet
                                   company-active-map))
      (add-hook 'company-mode-hook #'company-yas-tab)))
  (setq yas-triggers-in-field t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local yas-indent-line 'fixed)
              (setq-local yas-also-auto-indent-first-line 'nil))))

(provide 'base-editing)
;;; base-editing.el ends here
