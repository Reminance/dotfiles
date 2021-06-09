;;; init-yasnippet.el --- Support yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 代码片段
(use-package yasnippet
  :ensure t
  :defer 2
  ; :config
  ; (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
  )

;; 大量可用的代码片段
(use-package
  yasnippet-snippets
  :ensure t
  :after yasnippet)


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
