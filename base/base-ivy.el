;;; base-ivy.el --- panda's emacs base ivy file

;;; Commentary:
;;; bamboo

;;; Code:

(use-package ivy
  :ensure counsel
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper)
         ("C-c s" . counsel-rg)
         ("C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done))
  :init
  (ivy-mode 1)
  :config
  (eval-after-load 'projectile
    '(setq projectile-completion-system 'ivy))
  (setq ivy-wrap t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        confirm-nonexistent-file-or-buffer t
        ivy-count-format "(%d/%d) ")
  (defvar panda/ivy-match-colors '("#39FF14" "#67C8FF" "#FF9933")
    "The colors to use for ivy's match highlighting.")
  (defun panda/set-ivy-minibuffer-match-faces (match-colors)
    "Changes ivy minibuffer match faces based on MATCH-COLORS."
    (let* ((ivy-minibuffer-match-faces '(ivy-minibuffer-match-face-2
                                         ivy-minibuffer-match-face-3
                                         ivy-minibuffer-match-face-4))
           (ivy-face-count (length ivy-minibuffer-match-faces))
           (minibuffer-match-color-count (length match-colors)))
      (dotimes (n ivy-face-count)
        (set-face-attribute (nth n ivy-minibuffer-match-faces) nil
                            :foreground (nth (mod n minibuffer-match-color-count) match-colors)
                            :weight 'bold))))
  (panda/set-ivy-minibuffer-match-faces panda/ivy-match-colors)
  (set-face-attribute 'ivy-confirm-face nil
                      :foreground "#39FF14"))

(provide 'base-ivy)
;;; base-ivy.el ends here
