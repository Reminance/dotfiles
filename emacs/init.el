;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; init variables
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines nil)

;; 关闭备份
(setq make-backup-files nil auto-save-default nil)
;; 关闭多编辑器同时编辑统一文件时锁文件操作
(setq create-lockfiles nil)
;; 随时重新加载发生修改过的文件
(setq load-prefer-newer t)
;; 关闭字体缓存gc
(setq inhibit-compacting-font-caches nil)
;; 关闭烦人的提示
(setq ring-bell-function 'ignore blink-cursor-mode nil)

;; 任何地方都使用UTF-8
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)    ; pretty
(set-terminal-coding-system  'utf-8)    ; pretty
(set-keyboard-coding-system  'utf-8)    ; pretty
(set-selection-coding-system 'utf-8)    ; please
(prefer-coding-system        'utf-8)    ; with sugar on top
(setq default-process-coding-system '(utf-8 . utf-8))

;; 关闭自动调节行高
(setq auto-window-vscroll nil)
;; 关闭自动换行的功能
(setq truncate-partial-width-windows nil)

;; 创建新行的动作
;; 回车时创建新行并且对齐
(global-set-key (kbd "RET") 'newline-and-indent)
;; 取消对齐创建的新行
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; 让光标无法离开视线
(setq mouse-yank-at-point nil)

;; 最大单行字符数量
(setq-default fill-column 80)

;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda ()
                                          (modify-syntax-entry ?- "w")))
;; 允许插入制表符
(setq-default indent-tabs-mode nil)
;; 制表符宽度
(setq-default tab-width 4)

;; 高亮对应的括号
(show-paren-mode 1)

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
      scroll-margin 4
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)
;; init message
(setq-default initial-scratch-message
              (concat ";; Life is either a daring adventure, or nothing; " user-login-name ", Emacs ♥ you!\n\n"))
;; 设置光标样式
(setq-default cursor-type 'bar)
;; 高亮当前行
(global-hl-line-mode 1)

;; 设置英文/中文字体
                                        ;(setq reminance/en-font-name "Fira Code Nerd Font Mono"
(setq reminance/en-font-name "Iosevka"
      reminance/en-font-style "Regular"
      reminance/en-font-size 14)
                                        ;(setq reminance/zh-font-name "WenQuanYi Zen Hei Mono"
                                        ;(setq reminance/zh-font-name "Fira Code Nerd Font Mono"
(setq reminance/zh-font-name "Iosevka"
      reminance/zh-font-style "Regular"
      reminance/zh-font-size 14)
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

;; 显示跟踪空白
(setq-local show-trailing-whitespace t)

;;----------------------------------------------------------------------------
;; global common keybindings
;;----------------------------------------------------------------------------
;; some custom shortcut
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "\e\ei")
                (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "\e\el")
                (lambda () (interactive) (find-file (expand-file-name "lisp" user-emacs-directory))))


;;----------------------------------------------------------------------------
;; custom common function
;;----------------------------------------------------------------------------
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

;; for using with i3 keybinding, like:
;;     bindsym $mod+Ctrl+c exec "emacsclient -ne '(make-capture-frame)'"
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

;;----------------------------------------------------------------------------
;; basic configuation
;;----------------------------------------------------------------------------
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)
;; 开启行号
(global-display-line-numbers-mode +1)
;; 选中文本后输入会覆盖
(delete-selection-mode +1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

                                        ; (global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (reminance/toggle-transparency)))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (reminance/toggle-proxy)))

;;;###autoload
(defun reminance/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; melpa package
;;----------------------------------------------------------------------------
(require 'package)
(require 'cl-lib)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;;; Standard package repositories
;; (add-to-list 'package-archives '( "melpa" . "http://melpa.org/packages/") t)
;; (setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
;;                           ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/ ")
;;                           ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
;;                           ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(setq package-archives '(
                         ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
                         ("org" . "http://mirrors.tuna.tsinghuna.edu.cn/elpa/org/")
                         ))

;;; Fire up package.el
(package-initialize) ;; You might already have this line

;; Official MELPA Mirror, in case necessary.
;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

;; for try
(use-package try
  :ensure t)

;; evil
(use-package evil
;;  :disabled
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
;;  :disabled
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; magit
(use-package magit
  :ensure t
  :commands (magit))

;; 显示当前行修改-Git
;; (use-package git-gutter-fringe
;;              ;; :disabled
;;              :ensure nil
;;              :hook (prog-mode . git-gutter-mode)
;;              :custom
;;              (git-gutter:update-interval 1)
;;              (git-gutter:added-sign "+")
;;              (git-gutter:deleted-sign "_")
;;              (git-gutter:modified-sign "~")
;;              (git-gutter:hide-gutter t))
;; 
;; ;; 高亮修改记录
;; (use-package diff-hl
;;              :ensure t
;;              :hook ((prog-mode  . diff-hl-mode)
;;                     (dired-mode . diff-hl-dired-mode)) )

                                        ; (use-package markdown-mode
                                        ;   :ensure t
                                        ;   :mode "\\.md\\'"
                                        ;   :mode "\\.text\\'"
                                        ;   :mode "\\.markdown\\'")
;; (use-package markdown-mode+
;;              :defer t
;;              :ensure t
;;              :hook (markdown-mode . (lambda () (require 'markdown-mode+)))
;;              :config
;;              ())

;; 著名的Emacs补全框架
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :init (setq company-tooltip-align-annotations t company-idle-delay 0.1 company-echo-delay 0
              company-minimum-prefix-length 2 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t)
  :config
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous))
                                        ; (:map leader-key
                                        ;       ("c s" . #'company-yasnippet))
  )

;; 美化company
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; 代码片段
(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (setq yas-snippet-dirs '(expand-file-name "etc/snippets" user-emacs-directory)))

;; 大量可用的代码片段
(use-package yasnippet-snippets 
  :ensure t
  :after yasnippet)

;; 编译运行当前文件
(use-package quickrun
  :ensure t
  :commands(quickrun)
  ;; :bind (:map leader-key
  ;;             ("c r" . #'quickrun))
  :init (setq quickrun-timeout-seconds nil)
  (setq quickrun-focus-p nil)
  (setq quickrun-input-file-extension nil)
  :config
  (quickrun-add-command "python"
    '((:command .
                "python3")
      (:exec .
             "%c %s")
      (:tempfile .
                 nil))
    :default "python")
  (quickrun-add-command "c++/c1z"
	'((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
				   "%e %a"))
      (:remove  . ("%e")))
	:default "c++"))


;; 人工智能补全代码
(use-package company-tabnine
  :disabled
  :ensure t
  :after 'company-mode
  'company-tabnine-mode
  :config (add-to-list 'company-backends #'company-tabnine))

;; lsp
;; =================================================
;; = LSP-MODE SERVERS INSTALLATION INSTRUCTIONS    =
;; =================================================
;; = + bash                                        =
;; =   > `npm i -g bash-language-server'           =
;; = + python                                      =
;; =   > `pip install python-language-server[all]' =
;; = + ruby                                        =
;; =   > `gem install solargraph'                  =
;; = + java                                        =
;; =   > `(use-package lsp-java :ensure t)'        =
;; = + c/c++                                       =
;; =   > `sudo pacman -S ccls'                     =
;; =================================================
;; (use-package lsp-mode
;;   :ensure t
;;   :defer 2
;;   :commands (lsp)
;;   :hook ((java-mode js-mode js2-mode web-mode c-mode c++-mode objc-mode python-mode rust-mode) . lsp)
;;   :custom
;;   (lsp-idle-delay 200)
;;   (lsp-auto-guess-root nil)
;;   (lsp-file-watch-threshold 2000)
;;   (read-process-output-max (* 1024 10240))
;;   (lsp-eldoc-hook nil)
;;   (lsp-prefer-flymake nil)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-f" . lsp-format-buffer)
;;               ("M-RET" . lsp-ui-sideline-apply-code-actions)
;;               ("M-RET" . lsp-execute-code-action)
;;               )
;;   :config
;;   (setq lsp-prefer-capf t))


;; 各个语言的Debug工具
;; (use-package dap-mode
;;              :ensure t
;;              :functions dap-hydra/nil
;;              :diminish
;;              :bind (:map lsp-mode-map
;;                          ("<f5>" . dap-debug)
;;                          ("M-<f5>" . dap-hydra))
;;              :hook ((after-init . dap-mode)
;;                     (dap-mode . dap-ui-mode)
;;                     (python-mode . (lambda () (require 'dap-python)))
;;                     ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))))

;; 美化lsp-mode
;; (use-package lsp-ui
;;              :ensure t
;;              :hook (lsp-mode . lsp-ui-mode)
;;              :config
;;              ;; sideline
;;              (setq lsp-ui-sideline-show-diagnostics t
;;                    lsp-ui-sideline-show-hover t
;;                    lsp-ui-sideline-show-code-actions nil
;;                    lsp-ui-sideline-update-mode 'line
;;                    ;; sideline
;;                    lsp-ui-sideline-delay 1)
;;              ;; peek
;;              (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;              (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;              ;; doc
;;              (setq lsp-ui-doc-enable t
;;                    ;; 文档显示的位置
;;                    lsp-ui-doc-position 'top
;;                    ;; 显示文档的延迟
;;                    lsp-ui-doc-delay 2))

;; lsp-java
;; (use-package lsp-java
;;   :commands (lsp)
;;   :ensure t
;;   :hook (java-mode . (lambda () (require 'lsp-java)))
;;   :config
;;   (setq lsp-java-server-install-dir (expand-file-name "var/jdt-lsp" user-emacs-directory)))

;; flycheck
;; (use-package flycheck
;;              :ensure t
;;              :commands (flycheck-mode)
;;              ;; :hook (prog-mode . flycheck-mode)
;;              ; :bind (:map leader-key
;;              ;             ("t t" . global-flycheck-mode))
;;              :config (which-key-add-key-based-replacements "M-SPC t t" "开关flycheck")
;;              (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode org-mode diff-mode
;;                                                shell-mode eshell-mode term-mode vterm-mode)
;;                    flycheck-emacs-lisp-load-path 'inherit
;;                    ;; Only check while saving and opening files
;;                    ;; 只在打开和保存文件时才进行检查
;;                    flycheck-check-syntax-automatically '(save mode-enabled) flycheck-indication-mode
;;                    'right-fringe)
;;              ;; 美化一下
;;              (when (fboundp 'define-fringe-bitmap)
;;                (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil
;;                                      'center)))

;; 用GUI tooltips来显示检查到的错误
;; (progn
;;   (use-package flycheck-posframe
;;                :ensure t
;;                :custom-face (flycheck-posframe-border-face ((t
;;                                                               (:inherit default))))
;;                :hook (flycheck-mode . flycheck-posframe-mode)
;;                :init (setq flycheck-posframe-border-width 1 flycheck-posframe-inhibit-functions '((lambda
;;                                                                                                     (&rest _)
;;                                                                                                     (bound-and-true-p
;;                                                                                                       company-backend)))))
;;   (use-package flycheck-pos-tip
;;                :ensure t
;;                :defines flycheck-pos-tip-timeout
;;                :hook (global-flycheck-mode . flycheck-pos-tip-mode)
;;                :config (setq flycheck-pos-tip-timeout 30))
;;   (use-package flycheck-popup-tip
;;                :ensure t
;;                :hook (flycheck-mode . flycheck-popup-tip-mode)))

;; 写js可用的模式
;; (use-package js2-mode
;;              :ensure t)

;; 窗口管理器
(use-package windmove
  :defer 0
  :ensure t
                                        ; :init (windmove-default-keybindings)
                                        ; :config
                                        ; :bind (:map leader-key
                                        ;             ("w f" . #'windmove-right)
                                        ;             ("w b" . #'windmove-left)
                                        ;             ("w p" . #'windmove-up)
                                        ;             ("w n" . #'windmove-down)
                                        ;             ("w F" . #'window-move-right)
                                        ;             ("w B" . #'window-move-left)
                                        ;             ("w P" . #'window-move-up)
                                        ;             ("w N" . #'window-move-down)
                                        ;             ("w h" . #'enlarge-window-horizontally)
                                        ;             ("w l" . #'shrink-window-horizontally)
                                        ;             ("w j" . #'enlarge-window)
                                        ;             ("w k" . #'shrink-window))
  )

;; 项目管理
(use-package projectile
  :ensure t)

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))

  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient)))

  (evil-leader/set-key-for-mode 'restclient-mode
                                "mn" 'restclient-jump-next
                                "mp" 'restclient-jump-prev
                                "ms" 'restclient-http-send-current-stay-in-window
                                "mS" 'restclient-http-send-current
                                "mr" 'spacemacs/restclient-http-send-current-raw-stay-in-window
                                "mR" 'restclient-http-send-current-raw
                                "my" 'restclient-copy-curl-command))

;; 切换buffer焦点时高亮动画
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

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

;; icons font
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
    :custom (emojify-emojis-dir (expand-file-name "var/emojis" user-emacs-directory))))

;;   ;; 浮动窗口支持
;;   (use-package posframe
;;                :ensure t
;;                :custom
;;                (posframe-mouse-banish nil))

;; 彩虹猫进度条
;; (use-package nyan-mode
;;              :ensure t
;;              :hook (after-init . nyan-mode)
;;              :config
;;              (setq nyan-wavy-trail t
;;                    nyan-animate-nyancat t))

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

;; 命令日志
(use-package command-log-mode
  :ensure t)

;; themes config
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;  (load-theme 'doom-Iosvkem t)
;;  (load-theme 'doom-one t)
  (load-theme 'doom-snazzy t)
;;  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; for lisp
;; (use-package sly
;;   :hook (common-lisp-mode . sly-edit-mode) 
;;   :ensure t)
;; (use-package sly-macrostep
;;   :ensure t
;;   :hook (common-lisp-mode . sly-macrostep-mode))
;; 
;; (use-package sly-repl-ansi-color
;;   :ensure t
;;   :hook (common-lisp-mode . sly-repl-ansi-color))

;; for elisp
;; (use-package lispy
;;   :ensure t
;;   :hook (emacs-lisp-mode . lispy-mode))

;; lisp符号操作工具
;; (use-package symbol-overlay
;;   :ensure t
;;   :hook (emacs-lisp-mode . symbol-overlay-mode))

;; 增强*help* buffer的功能
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

;; 为*help*中的函数提供elisp例子
(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; 关闭鼠标功能
(use-package disable-mouse
  :ensure t
  :hook (after-init . (lambda ()
                        (global-disable-mouse-mode -1))))

;; 管理员模式编辑
(use-package sudo-edit
  :ensure t)

;; 括号匹配
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode))

;; wgrep
(setq-default grep-highlight-matches t
              grep-scroll-output t)
(when *is-a-mac*
  (setq-default locate-command "mdfind"))
(use-package wgrep
  :ensure t
  :config
  (setq-default locate-command "mdfind")
  )
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

;; 回到关闭文件前光标的位置
(use-package saveplace
  :ensure t
  :hook (after-init . (lambda () (save-place-mode t))))

;; 键位提示
(use-package which-key
  :ensure t
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode 1))

;; 如果不喜欢ivy可以用这个包替换
(use-package selectrum
  ;; :disabled
  :ensure t
  :config
  (selectrum-mode +1)
  (use-package selectrum-prescient
    :ensure t
    :disabled
    :config
    (prescient-persist-mode +1)
    (selectrum-prescient-mode +1)))

;; 增强了搜索功能
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;; 集成了很多非常有用的的功能
(use-package counsel
  :ensure t
  :bind
  (("C-x C-r" . 'counsel-recentf)
   ("C-x d" . 'counsel-dired))
  :config
  ;; 默认的 rg 配置
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command (list "rg"
                                      "-M" "240"
                                      "--with-filename" "--no-heading" "--line-number" "--color"
                                      "never" "%s"
                                      "-g" "!package-config.org"
                                      "-g" "!site-lisp"
                                      "-g" "!doc"
                                      "-g" "!themes"
                                      "-g" "!quelpa"
                                      "-g" "!etc-cache"))
  (setq counsel-fzf-cmd "fd -I --exclude={site-lisp,etc/snippets,themes,/eln-cache,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/} --type f | fzf -f \"%s\" --algo=v1")
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  )

;; for hydra
;; (use-package hydra
;;   :defer 0
;;   :ensure t)
;;
;; (use-package hydra-posframe
;;   :quelpa ((hydra-posframe
;;             :fetcher github
;;             :repo "Ladicle/hydra-posframe"))
;;   :hook (after-init . (lambda ()
;;                         (hydra-posframe-mode +1) ) ))
;;
;; (use-package
;;   major-mode-hydra
;;   :defer 0
;;   :ensure t
;;   :after hydra)

;; 安装quelpa包管理器（用于安装github上的插件）
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade))
;;   (quelpa
;;    '(quelpa-use-package
;;      :fetcher git
;;      :url "https://github.com/quelpa/quelpa-use-package.git")))
;; (require 'quelpa-use-package)

;; (setq quelpa-upgrade-interval 7
;;       quelpa-update-melpa-p nil
;;       use-package-ensure-function 'quelpa
;;       use-package-always-ensure t)
;; (setq use-package-always-defer t)

;;; org
;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

;; 美化org
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  ;;   :custom (org-bullets-bullet-list '("☰" "☷" "✿" "☭"))
  )

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
                                        ; (require 'init-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
                                        ; (require 'init-local nil t)


(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
