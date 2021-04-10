;;; init-lsp.el --- Support lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :ensure t
  :defer 2
  :commands (lsp)
  :hook ((java-mode js-mode js2-mode web-mode c-mode c++-mode objc-mode python-mode rust-mode) . lsp)
  :custom
  (lsp-idle-delay 200)
  (lsp-auto-guess-root nil)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 10240))
  (lsp-eldoc-hook nil)
  (lsp-prefer-flymake nil)
  :bind (:map lsp-mode-map
              ("C-c C-f" . lsp-format-buffer)
              ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :config
  (setq lsp-prefer-capf t))


;; 各个语言的Debug工具
(use-package dap-mode
  :ensure t
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda () (require 'dap-python)))
         ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))))


;; 美化lsp-mode
(use-package
  lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; sideline
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-update-mode 'line
        ;; sideline
        lsp-ui-sideline-delay 1)
  ;; peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; doc
  (setq lsp-ui-doc-enable t
        ;; 文档显示的位置
        lsp-ui-doc-position 'top
        ;; 显示文档的延迟
        lsp-ui-doc-delay 2))


(provide 'init-lsp)
;;; init-lsp.el ends here
