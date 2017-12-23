;;; base-ivy.el --- panda's emacs base ivy file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ivy
  :ensure counsel
  :diminish ivy-mode
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper)
         ("C-c s" . counsel-rg)
         ("C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done))
  :config
  (ivy-mode 1)
  (setq ivy-wrap t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        confirm-nonexistent-file-or-buffer t
        ivy-count-format "(%d/%d) ")
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :foreground panda/neon-green
                      :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :foreground panda/light-blue
                      :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :foreground panda/deep-saffron
                      :weight 'bold)
  (set-face-attribute 'ivy-confirm-face nil
                      :foreground panda/neon-green))

(provide 'base-ivy)
;;; base-ivy.el ends here
