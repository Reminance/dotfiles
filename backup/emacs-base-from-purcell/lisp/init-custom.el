;;; init-custom.el --- Support for the custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 设置光标样式
(setq-default cursor-type 'bar)

;; 高亮当前行
(global-hl-line-mode 1)

;; some custom shortcut
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "\e\ei")
                (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "\e\el")
                (lambda () (interactive) (find-file (expand-file-name "lisp" user-emacs-directory))))

;; 切换代理
(defun reminance/toggle-proxy ()
  (interactive)
  (if (null url-proxy-services)
      (progn
        (setq url-proxy-services
              '(("http" . "127.0.0.1:7890")
                ("https" . "127.0.0.1:7890")))
        (message "proxy on."))
    (setq url-proxy-services nil)
    (message "proxy off.")))

;; 设置英文/中文字体
; (setq reminance/en-font-name "Fira Code Nerd Font Mono"
(setq reminance/en-font-name "Iosevka"
      reminance/en-font-style "Regular"
      reminance/en-font-size 16)

; (setq reminance/zh-font-name "WenQuanYi Zen Hei Mono"
(setq reminance/zh-font-name "Iosevka"
      reminance/zh-font-style "Regular"
      reminance/zh-font-size 16)

(progn
  (if (fontp (font-spec
              :name reminance/en-font-name
              :style reminance/en-font-style
              :size reminance/en-font-size))
      (progn
        (set-face-attribute 'default nil
                            :font (font-spec
                                   :name reminance/en-font-name
                                   :style reminance/en-font-style
                                   :size reminance/en-font-size))
        (set-fontset-font t 'han (font-spec
                                  :name reminance/zh-font-name
                                  :style reminance/zh-font-style))
        (set-fontset-font "fontset-default" ?༼ (font-spec
                                                :name "Noto Serif Tibetan"
                                                :size 0)))
    (message "Can't find %s font. You can install it or ignore this message at init-font.el" reminance/en-font-name)))

(use-package try
             :ensure t)

(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Emacs下最好用的终端仿真器
(use-package vterm
  :commands (vterm)
  :ensure t
  :bind (:map leader-key
              ("o t" . 'vterm)))

;; 有道词典，非常有用
(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :ensure t 
  :config (setq url-automatic-caching t) 
  (which-key-add-key-based-replacements "C-x y" "有道翻译")
  :bind (("C-x y t" . 'youdao-dictionary-search-at-point+)
         ("C-x y g" . 'youdao-dictionary-search-at-point-posframe)
         ("C-x y p" . 'youdao-dictionary-play-voice-at-point)
         ("C-x y r" . 'youdao-dictionary-search-and-replace)
         ("C-x y i" . 'youdao-dictionary-search-from-input)))

;; 主题包
(use-package doom-themes
  :ensure t
  :config)

(progn
  (use-package all-the-icons
               :ensure t)
  ;; dired模式图标支持
  (use-package all-the-icons-dired
               :ensure t
               :hook ('dired-mode . 'all-the-icons-dired-mode))
  ;; 表情符号
  (use-package emojify
               :ensure t
               :custom (emojify-emojis-dir "~/.emacs.d/var/emojis"))
  ;; 浮动窗口支持
  (use-package posframe
               :ensure t
               :custom
               (posframe-mouse-banish nil)))

;; 彩虹猫进度条
(use-package nyan-mode
             :ensure t
             :hook (after-init . nyan-mode)
             :config
             (setq nyan-wavy-trail t
                   nyan-animate-nyancat t))

;; ASCII艺术字
(use-package figlet
  :ensure t
  :config
  (setq figlet-default-font "standard"))

;; 撤销树
(use-package undo-tree
  :ensure t
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t undo-tree-enable-undo-in-region nil undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings (make-variable-buffer-local 'undo-tree-visualizer-diff)
                    (setq-default undo-tree-visualizer-diff t)))

;; 看英语文档神器
; (use-package english-teacher
;   :quelpa ((english-teacher :fetcher github :repo "loyalpartner/english-teacher.el"))
;   :custom
;   (english-teacher-backend 'baidu)
;   (english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
;   :hook ((Info-mode
;            elfeed-show-mode
;            Man-mode
;            Woman-mode
;            ;; help-mode
;            ) . english-teacher-follow-mode))

;; 用posframe在dired模式下显示文件内容
(use-package dired-posframe
  :ensure t
  :custom
  (dired-posframe-size-limit (* 100 1024 1024))
  :bind((:map dired-mode-map)
        ("C-*" . dired-posframe-mode)))

;; 美化org
(use-package
  org-bullets
  :ensure t
  :after org
  :hook ('org-mode . 'org-bullets-mode)
  ; :custom
  ; (org-bullets-bullet-list '("☰" "☷" "✿" "☭"))
  )

;; 写网页可用的模式
(use-package
  web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :hook (html-mode . web-mode))

(use-package noflet
             :ensure t)

 (defun make-capture-frame ()
     "Create a new frame and run org-capture."
     (interactive)
     (make-frame '((name . "capture")))
     (select-frame-by-name "capture")
     (delete-other-windows)
     (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
       (org-capture)))

(provide 'init-custom)
;;; init-custom.el ends here
