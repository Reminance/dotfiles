;;; init-java.el --- Support Java mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package lsp-java
  :commands (lsp)
  :ensure t
  :hook (java-mode . (lambda () (require 'lsp-java)))
  :config
  (setq lsp-java-server-install-dir (expand-file-name "var/jdt-lsp" user-emacs-directory)))


(provide 'init-java)
;;; init-java.el ends here
