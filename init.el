;;; init.el --- emacs init file

;;; Commentary:
;;; See config.org for the configuration.

;;; Code:

(defconst org-config-path
  (expand-file-name "config.org" user-emacs-directory))

(defconst el-config-path
  (expand-file-name "config.el" user-emacs-directory))

(if (file-newer-than-file-p org-config-path el-config-path)
    (org-babel-load-file org-config-path)
  (load-file el-config-path))

(provide 'init)
;;; init.el ends here
