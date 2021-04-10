;;; init-variables.el --- for the variables -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 设置eshell历史记录
(setq eshell-history-file-name (expand-file-name "var/eshell/history" user-emacs-directory))
(setq recentf-save-file  (expand-file-name "var/recentf" user-emacs-directory))
;; 自动刷新被修改过的文件
(global-auto-revert-mode +1)
(setq desktop-dirname  (expand-file-name "var/desktop-save" user-emacs-directory))
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix  (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))
;; minibuffer history
(setq savehist-file (expand-file-name "var/savehist" user-emacs-directory))
;; 更友好及平滑的滚动
(setq scroll-step 2
      scroll-margin 6
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

(setq-default initial-scratch-message
              (concat ";; Life is either a daring adventure, or nothing; " user-login-name ", Emacs ♥ you!\n\n"))


(provide 'init-variables)
;;; init-variables.el ends here
