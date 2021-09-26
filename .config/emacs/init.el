;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error nil)

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

;;----------------------------------------------------------------------------
;; init variables
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-cocoa-emacs* (and *is-mac* (eq window-system 'ns)))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-x11* (eq window-system 'x))
(defconst *is-windows* (eq system-type 'windows-nt))
(when *is-mac*
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
  ;; what describe-key reports for cmd-option-h
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))

(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

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
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

;; 关闭自动调节行高
(setq auto-window-vscroll nil)
;; 关闭自动换行的功能
(setq truncate-partial-width-windows nil)

;; 最大单行字符数量
(setq-default fill-column 80)
;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w")))
;; 允许插入制表符
(setq-default indent-tabs-mode nil)
;; 制表符宽度
(setq-default tab-width 4)
;; 显示跟踪空白
(setq-local show-trailing-whitespace t)

;; 高亮对应的括号
(show-paren-mode 1)

;; 设置eshell历史记录
(setq url-configuration-directory (expand-file-name "var/url" user-emacs-directory))
(setq eshell-history-file-name (expand-file-name "var/eshell/history" user-emacs-directory))
(setq recentf-save-file  (expand-file-name "var/recentf" user-emacs-directory))
;; 自动刷新被修改过的文件
(global-auto-revert-mode +1)
(setq desktop-dirname  (expand-file-name "var/desktop-save" user-emacs-directory))
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix  (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))
;; minibuffer history
(setq savehist-file (expand-file-name "var/savehist" user-emacs-directory))
;; emacs bookmarks
(setq bookmark-default-file (expand-file-name "var/bookmarks" user-emacs-directory))

;; 更友好及平滑的滚动
(setq scroll-conservatively 100)

;; init message
;; (setq-default initial-scratch-message
;;               (concat ";; Hi, " user-login-name ", Emacs ♥ you!\n\n"))

;; 设置光标样式
;; (setq-default cursor-type t)

;; 高亮当前行
(global-hl-line-mode 1)

;; 设置英文/中文字体
;; (setq my/en-font-name "Fira Code Nerd Font Mono"
(setq my/en-font-name "Iosevka"
      my/en-font-style "Regular"
      my/en-font-size 16)
;; (setq my/zh-font-name "WenQuanYi Zen Hei Mono"
;; (setq my/zh-font-name "Fira Code Nerd Font Mono"
(setq my/zh-font-name "Iosevka"
      my/zh-font-style "Regular"
      my/zh-font-size 16)
(progn
  (if (fontp (font-spec
              :name my/en-font-name
              :style my/en-font-style
              :size my/en-font-size))
      (progn
        (set-face-attribute 'default nil
                            :font (font-spec
                                   :name my/en-font-name
                                   :style my/en-font-style
                                   :size my/en-font-size))
        (set-fontset-font t 'han (font-spec
                                  :name my/zh-font-name
                                  :style my/zh-font-style))
        (set-fontset-font "fontset-default" ?༼ (font-spec
                                                :name "Noto Serif Tibetan"
                                                :size 0)))
    (message "Can't find %s font. You can install it or ignore this message at init-font.el" my/en-font-name)))

;;----------------------------------------------------------------------------
;; global common keybindings
;;----------------------------------------------------------------------------
;; some custom shortcut
;; (global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "C-z"))
(define-prefix-command 'leader-key)
(global-set-key (kbd "M-SPC") 'leader-key)
(define-key leader-key "<f5>" 'revert-buffer)
(define-key leader-key "fi" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key leader-key "fn" (lambda () (interactive) (find-file "~/doc/org/notes.org")))
(define-key leader-key "fp" (lambda () (interactive) (find-file "~/doc/org/personal.org")))
(define-key leader-key "fw" (lambda () (interactive) (find-file "~/doc/org/work.org")))
(define-key leader-key "al" 'org-agenda-list)
(define-key leader-key "e" 'mu4e)

;;----------------------------------------------------------------------------
;; custom common function
;;----------------------------------------------------------------------------
(defun my/toggle-proxy ()
  "Toggle-proxy."
  (interactive)
  (if (null url-proxy-services)
      (progn
        (setq url-proxy-services
              '(("http" . "192.168.0.101:7890")
                ("https" . "192.168.0.101:7890")))
        (message "proxy on."))
    (setq url-proxy-services nil)
    (message "proxy off.")))

;; for using with i3 keybinding, like:
;;     bindsym $mod+Ctrl+c exec "emacsclient -ne '(make-capture-frame)'"
;; (use-package noflet)
;; (defun make-capture-frame ()
;;   "Create a new frame and run 'org-capture'."
;;   (interactive)
;;   (make-frame '((name . "capture")))
;;   (select-frame-by-name "capture")
;;   (delete-other-windows)
;;   (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf))) (org-capture)))

;;----------------------------------------------------------------------------
;; basic configuation
;;----------------------------------------------------------------------------
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Window size and features
;; (setq-default
;;  window-resize-pixelwise t
;;  frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; enable mouse in terminal
(xterm-mouse-mode 1)

;; 开启行号
;; (setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; show in mode line
;; show modeline column number
(setq column-number-mode t)
;; Toggle buffer size display in the mode line (Size Indication mode).
(size-indication-mode 1)

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

;;(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key (kbd "C-M-7") (lambda () (interactive) (my/toggle-transparency)))
(global-set-key (kbd "C-M-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "C-M-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "C-M-0") (lambda () (interactive) (my/toggle-proxy)))

;;;###autoload
;; Make frame transparency overridable
(defvar my/frame-transparency '(90 . 90))
(defun my/toggle-transparency ()
  "Toggle-transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         my/frame-transparency '(100 . 100)))))

;; Set frame transparency at startup
;; (set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; split window and change focus
(defadvice split-window (after move-point-to-new-window activate)
  "Move the point to the newly created window after splitting."
  (other-window 1))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; melpa package
;;----------------------------------------------------------------------------
(require 'package)
;; (require 'cl-lib)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;;; Standard package repositories
;; (add-to-list 'package-archives '( "melpa" . "http://melpa.org/packages/") t)
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                          ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;;                          ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
;;                          ("org" . "http://mirrors.tuna.tsinghuna.edu.cn/elpa/org/")
;;                          ))
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/stable-melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ))

;;; Fire up package.el
(package-initialize) ;; You might already have this line

;; Official MELPA Mirror, in case necessary.
;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; (when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; windmove 窗口管理器
(use-package windmove
  :init (windmove-default-keybindings)
  :config (use-package buffer-move)
  :bind (
         ("C-M-<left>" . 'windmove-left)
         ("C-M-<down>" . 'windmove-down)
         ("C-M-<up>" . 'windmove-up)
         ("C-M-<right>" . 'windmove-right)
         ("C-M-S-<left>" . 'windmove-swap-states-left)
         ("C-M-S-<down>" . 'windmove-swap-states-down)
         ("C-M-S-<up>" . 'windmove-swap-states-up)
         ("C-M-S-<right>" . 'windmove-swap-states-right)
         ;; ("C-M-S-h" . #'shrink-window-horizontally)
         ;; ("C-M-S-j" . #'enlarge-window)
         ;; ("C-M-S-k" . #'shrink-window)
         ;; ("C-M-S-l" . #'enlarge-window-horizontally)
         ))

;; Use Ibuffer for Buffer List
(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        (quote (("home"
                 ("workspace" (filename . "workspace"))
                 ("dotfiles" (or (filename . ".dotfiles")))
                 ("mu4e" (or (filename . "\*mu4e\*")))
                 ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
	             ("Org" (or (mode . org-mode) (filename . "OrgMode")))
	             ("Magit" (name . "\*magit"))
	             ("Help" (or (name . "\*Help\*") (name . "\*Apropos\*") (name . "\*info\*")))
                 ))))
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode)
	           (ibuffer-switch-to-saved-filter-groups "home")))
  :bind ([remap list-buffers] . ibuffer))
(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-q") 'delete-window)
;; (global-set-key (kbd "C-x b") 'ibuffer)

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char)
  ("M-S" . avy-goto-char-2)
  )

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; try
(use-package try)

;; evil
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (eval-after-load "evil-maps"
    (dolist (map '(evil-motion-state-map
                   evil-normal-state-map
                   evil-insert-state-map
                   evil-visual-state-map
                   evil-emacs-state-map))
      (define-key (eval map) "\C-n" nil)
      (define-key (eval map) "\C-p" nil)
      (define-key (eval map) "\C-a" nil)
      (define-key (eval map) "\C-e" nil)
      (define-key (eval map) "\C-f" nil)
      (define-key (eval map) "\C-b" nil)
      (define-key (eval map) "\C-y" nil)
      (define-key (eval map) "\C-k" nil)
      (define-key (eval map) "\C-u" nil)
      (define-key (eval map) "\C-d" nil)
      (define-key (eval map) "\C-w" nil)
      (define-key (eval map) "\C-z" nil)
      (define-key (eval map) "\M-." nil)
      (define-key (eval map) (kbd "TAB") nil)
      )
    )
  (eval-after-load "evil-maps"
    (dolist (map '(
                   evil-insert-state-map
                   ))
      (define-key (eval map) "\C-i" nil) ;; evil-jump-forward didn't work, cause it translate into tab in magit
      (define-key (eval map) "\C-o" nil)
      (define-key (eval map) "\C-v" nil)
      )
    )
  (setq evil-disable-insert-state-bindings t)
  (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; magit
(use-package magit
  :commands (magit)
  )

;; 显示当前行修改-Git
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :custom
  (git-gutter:update-interval 0)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:modified-sign "~")
  (git-gutter:hide-gutter t))

;; 著名的Emacs补全框架
(use-package company
  :hook (prog-mode . company-mode)
  :demand t
  :diminish company-mode
  ;; :bind (("C-<tab>" . company-complete)
  :bind (
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ;; ("SPC" . company-abort)
         )
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (setq company-idle-delay 0
          company-echo-delay 0
          company-tooltip-align-annotations t
          company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-show-numbers t
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-global-modes '(not magit-status-mode))
    ))

;; 美化company
(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

;; 代码片段
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/etc/snippets"))
  (yas-global-mode))

;; 大量可用的代码片段
(use-package yasnippet-snippets :after yasnippet)

(use-package multiple-cursors
  :bind (
         ("C-c M-N" . mc/edit-lines)
         ("M-N" . mc/mark-next-like-this)
         ("M-P" . mc/unmark-next-like-this)
         ;; ("M-P" . mc/mark-previous-like-this)
         ;; ("C-c C-<" . mc/mark-all-like-this)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)
         ))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

(setq kill-ring-max 100)
(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'rainbow-mode)
  (diminish 'yas-minor-mode)
  (diminish 'flycheck-mode)
  (diminish 'helm-mode))

;; 编译运行当前文件
(use-package quickrun
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
;; (use-package company-tabnine
;;   :disabled
;;   :after 'company-mode 'company-tabnine-mode
;;   :config (add-to-list 'company-backends #'company-tabnine))

;; 项目管理
(use-package projectile)

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package ob-restclient)
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))
  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient))))

;; (evil-leader/set-key-for-mode 'restclient-mode
;;                               "mn" 'restclient-jump-next
;;                               "mp" 'restclient-jump-prev
;;                               "ms" 'restclient-http-send-current-stay-in-window
;;                               "mS" 'restclient-http-send-current
;;                               "mr" 'spacemacs/restclient-http-send-current-raw-stay-in-window
;;                               "mR" 'restclient-http-send-current-raw
;;                               "my" 'restclient-copy-curl-command)

(require 'url-util)
(use-package ob-go
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (shell . t)
   (sql . t)
   (java . t)
   (go . t)
   (emacs-lisp . t)
   (shell . t)))
(global-set-key (kbd "C-c r") 'org-babel-remove-result)

;; 切换buffer焦点时高亮动画
;; (use-package beacon
;;   :hook (after-init . beacon-mode))

;; 有道词典，非常有用
(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :config (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-x y" "有道翻译")
  :bind (("C-c y y" . 'youdao-dictionary-search-at-point+)
         ("C-c y g" . 'youdao-dictionary-search-at-point-posframe)
         ("C-c y p" . 'youdao-dictionary-play-voice-at-point)
         ("C-c y r" . 'youdao-dictionary-search-and-replace)
         ("C-c y i" . 'youdao-dictionary-search-from-input)))

;; icons font
(progn
  (use-package all-the-icons)
  ;; dired模式图标支持
  (use-package all-the-icons-dired
    :hook ('dired-mode . 'all-the-icons-dired-mode))
  ;; 表情符号
  (use-package emojify
    :custom (emojify-emojis-dir (expand-file-name "var/emojis" user-emacs-directory))))

;; ASCII艺术字
(use-package figlet
  :config
  (setq figlet-default-font "standard"))

;; 撤销树
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t undo-tree-enable-undo-in-region nil undo-tree-auto-save-history nil)
  ;; HACK: keep the diff window
  (with-no-warnings (make-variable-buffer-local 'undo-tree-visualizer-diff)
                    (setq-default undo-tree-visualizer-diff t)))

;; 命令日志
(use-package command-log-mode)

;; themes config
;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-monokai-pro t)
;;   ;; (load-theme 'doom-molokai t)
;;   ;; (load-theme 'doom-gruvbox t)
;;   ;; (load-theme 'doom-Iosvkem t)
;;   ;; (load-theme 'doom-one t)
;;   ;; (load-theme 'doom-dracula t)
;;   ;; Enable flashing mode-line on errors
;;   ;; (doom-themes-visual-bell-config)
;;   )

;; (if (display-graphic-p)
;;     (load-theme 'doom-molokai t)
;;   ;; (load-theme 'doom-gruvbox t)
;;   ;; (load-theme 'doom-molokai t)
;;   (load-theme 'doom-monokai-pro t)
;;   )

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
(custom-set-faces
 '(line-number ((t (:background "gray20" :foreground "#6F6F6F"))))
 '(region ((t (:extend t :background "gray32"))))
 '(org-ellipsis ((t (:underline nil))))
 )
(let ((zenburn-background-color '(background-color . "gray20")))
  (add-to-list 'default-frame-alist zenburn-background-color)
  (add-to-list 'initial-frame-alist zenburn-background-color))

;; (use-package monokai-alt-theme
;;   :config
;;   (load-theme 'monokai-alt t)
;;   )
;; (use-package monokai-pro-theme
;;   :config
;;   (load-theme 'monokai-pro t)
;;   )
;; (use-package monokai-theme
;;   :config
;;   (load-theme 'monokai t)
;;   )
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox t)
;;   )
;; (use-package flatland-theme
;;   :config
;;   (load-theme 'flatland t)
;;   )
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-gruvbox t)
;;   ;; (load-theme 'doom-molokai t)
;;   ;; (load-theme 'doom-monokai-pro t)
;;   ;; (load-theme 'doom-xcode t)
;;   )

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))

;; 增强*help* buffer的功能
;; (use-package helpful
;;   :bind
;;   (("C-h f" . helpful-callable)
;;    ("C-h v" . helpful-variable)
;;    ("C-h k" . helpful-key)))

;; 为*help*中的函数提供elisp例子
;; (use-package elisp-demos
;;   :config
;;   (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; 关闭鼠标功能
;; (use-package disable-mouse
;;   :hook (after-init . (lambda ()
;;                         (global-disable-mouse-mode -1))))

;; 管理员模式编辑
(use-package sudo-edit)

;; 括号匹配
(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\})
                            (?\` . ?\`)
                            (?\" . ?\")
                            ))
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\<) t (electric-pair-default-inhibit c))))
(electric-pair-mode t)

;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode))

;; 回到关闭文件前光标的位置
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "var/places" "var/.emacs-places"))
  :hook (after-init . (lambda () (save-place-mode t))))

;; 键位提示
(use-package which-key
  :diminish which-key-mode
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode 1))

;; other similar packages to prescient: vertico consult selectrum prescient for completion

(use-package ivy
  :diminish ivy-mode
  :ensure t)

;; 增强了搜索功能
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; 集成了很多非常有用的的功能
(use-package counsel
  :bind
  (
   ("M-x" . 'counsel-M-x)
   ("C-x b" . 'counsel-ibuffer)
   ("C-M-j" . 'counsel-switch-buffer) ;; skip corrent buffer, more efficient
   ("C-x d" . 'counsel-dired)
   ("C-x C-f" . 'counsel-find-file)
   ("C-x C-r" . 'counsel-recentf)
   ("C-M-f" . counsel-rg)
   ("C-M-n" . counsel-fzf)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history)
   )
  :config
  ;; 默认的 rg 配置
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command "rg -i -M 240 --with-filename --no-heading --line-number --color never %s -g !doc -g !themes -g !quelpa")
  (setq counsel-fzf-cmd "fd -I --exclude={site-lisp,etc/snippets,themes,/eln-cache,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/} --type f | fzf -f \"%s\" --algo=v1")
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

;; 如果不喜欢ivy可以用这个包替换
;; (use-package selectrum :config (selectrum-mode +1))

;; (use-package selectrum-prescient
;;     :config
;;     (prescient-persist-mode +1)
;;     (selectrum-prescient-mode +1))

;; (use-package prescient
;;   :after counsel
;;   :config
;;   (prescient-persist-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (setq prescient-sort-length-enable nil)
  ;; This is the default value!
  (setq prescient-filter-method '(literal regexp fuzzy))
  ;; If you are too used to Ivy’s filtering styles, you can use those while still keeping Prescient’s sorting:
  (setq ivy-prescient-enable-filtering nil)
  ;; Getting the old highlighting back
  ;; (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode 1)
  ;; Remember candidate frequencies across sessions
  (prescient-persist-mode 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

;;; org
;;(image-type-available-p 'imagemagick) ;; It will evaluate to t if your Emacs has Imagemagick support.
;;(setq org-default-notes-file (concat org-directory "~/doc/org/notes.org"))
(use-package org
  :config
  ;; This can be solved by adding a hook to org-tab-first-hook which adds org-end-of-line.
  ;; Every time TAB is used it jumps to last visible character of the org-line, but before the ellipsis, and then opens/closes the container as usual.
  (add-hook 'org-tab-first-hook 'org-end-of-line)
  (setq org-ellipsis "▾")
  (eval-after-load 'org
    (progn
      (define-key org-mode-map (kbd "<C-M-S-right>") nil)
      (define-key org-mode-map (kbd "<C-M-S-left>") nil)))
  )

;; for test
(defun my/check-cursor-end-of-line ()
  "Check end of line."
  (interactive)
  (if
      (eq ?\n (char-after))
      (message "yes")
    (message "no")
    )
  )

;; Easy Templates support shortcuts such as: '<s + TAB'
(require 'org-tempo)

(global-set-key (kbd "C-c '") 'org-edit-src-code)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-confirm-evaluate nil)
(setq org-src-window-setup 'current-window)
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images nil)
(setq org-default-notes-file "~/doc/org/notes.org")
(setq org-agenda-files (list
                        "~/doc/org/notes.org"
                        "~/doc/org/personal.org"
                        "~/doc/org/work.org"
                        ))
(add-hook 'org-mode-hook 'org-indent-mode)

;;; Archiving
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

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
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
    ;; :custom (org-bullets-bullet-list '("☰" "☷" "✿" "☭"))
  )

;; flycheck
(use-package flycheck
  :commands (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :bind (:map leader-key
              ("t t" . global-flycheck-mode))
  :config (which-key-add-key-based-replacements "M-SPC t t" "开关flycheck")
  (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode org-mode diff-mode
                                    shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        ;; 只在打开和保存文件时才进行检查
        flycheck-check-syntax-automatically '(save mode-enabled) flycheck-indication-mode
        'right-fringe)
  ;; 美化一下
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil
      'center)))

;; for hydra
;; (use-package hydra :defer 0)
;; (use-package major-mode-hydra
;;   :defer 0
;;   :after hydra)
;; (use-package hydra-posframe
;;   :quelpa ((hydra-posframe
;;             :fetcher github
;;             :repo "Ladicle/hydra-posframe"))
;;   :hook (after-init . (lambda ()
;;                         (hydra-posframe-mode +1) ) ))

;; golang
(use-package go-mode
  :ensure t)
;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-save-hooks)

;; Start LSP Mode and YASnippet mode
;; (add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook 'lsp)

;; C/C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

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
;; =   > `(use-package lsp-java)'        =
;; = + c/c++                                       =
;; =   > `sudo pacman -S ccls'                     =
;; = + go
;; =   > `sudo pacman -S gopls'                     =
;; =================================================
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-C-l" . lsp-format-buffer)
              ("M-RET" . lsp-ui-sideline-apply-code-actions)
              ("M-RET" . lsp-execute-code-action))
  :config
  (setq lsp-completion-enable-additional-text-edit nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil)
  )

;; ;; 美化lsp-mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; sideline
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-delay 0.1
        )
  ;; peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; doc
  (if (display-graphic-p)
      (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-enable nil)
    )
  (setq lsp-ui-doc-position 'at-point        ;; 文档显示的位置
        lsp-ui-doc-delay 1        ;; 显示文档的延迟
        ))

;; ;; lsp-java
;; (use-package lsp-java
;;   :config
;;   (add-hook 'java-mode-hook 'lsp)
;;   (setq lsp-java-server-install-dir (expand-file-name "var/jdt-lsp" user-emacs-directory)))

;; ;; 各个语言的Debug工具
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)

;; ;; 写js可用的模式
;; (use-package js2-mode)

;; mu4e
;; (use-package mu4e
;;   :ensure nil
;;   :load-path "/usr/share/emacs/site-lisp/mu4e"
;;   :config
;;   (setq mu4e-change-filenames-when-moving t)
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   (setq mu4e-update-interval (* 5 60))
;;   (setq mu4e-maildir "~/email")
;;   (setq mu4e-attachment-dir "~/Downloads")
;;   (setq mu4e-compose-signature-auto-include nil)
;;   (setq mu4e-use-fancy-chars t)
;;   (setq mu4e-view-show-addresses t)
;;   (setq mu4e-view-show-images t)
;;   (add-to-list 'mu4e-bookmarks '("m:/qq/INBOX or m:/work/Inbox" "All Inboxes" ?i))
;;   (setq message-send-mail-function 'smtpmail-send-it)
;;   (setq mu4e-compose-context-policy 'ask-if-none)
;;   (setq mu4e-contexts
;;         (list
;;          ;; personal account
;;          (make-mu4e-context
;;           :name "qq"
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (string-match-p "^/qq" (mu4e-message-field msg :maildir))))
;;           :vars '((user-mail-address . "872666026@qq.com")
;;                   (user-full-name    . "xucheng")
;;                   (smtpmail-smtp-user     . "872666026@qq.com")
;;                   (smtpmail-smtp-server  . "smtp.qq.com")
;;                   (smtpmail-smtp-service . 465)
;;                   (smtpmail-stream-type  . ssl)
;;                   (mu4e-compose-signature . "xucheng via qq mail")
;;                   (mu4e-drafts-folder  . "/qq/Drafts")
;;                   (mu4e-sent-folder . "/qq/Sent Messages")
;;                   (mu4e-refile-folder . "/qq/Archive")
;;                   (mu4e-trash-folder  . "/qq/Deleted Messages")
;;                   (mu4e-maildir-shortcuts . (("/qq/INBOX" . ?i)
;;                                              ("/qq/Deleted Messages" . ?d)
;;                                              ("/qq/Drafts" . ?D)
;;                                              ("/qq/Sent Messages" . ?s)
;;                                              ("/qq/Junk" . ?j)))
;;                   ))
;;          ;; work account
;;          (make-mu4e-context
;;           :name "work"
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (string-match-p "^/work" (mu4e-message-field msg :maildir))))
;;           :vars '((user-mail-address . "xucheng2@shein.com")
;;                   (user-full-name . "xucheng2")
;;                   (smtpmail-smtp-user  . "xucheng2@shein.com")
;;                   (smtpmail-smtp-server  . "smtp.exmail.qq.com")
;;                   (smtpmail-smtp-service . 465)
;;                   (smtpmail-stream-type  . ssl)
;;                   (mu4e-compose-signature . "xucheng via qq exmail")
;;                   (mu4e-drafts-folder . "/work/Drafts")
;;                   (mu4e-sent-folder . "/work/Sent Messages")
;;                   (mu4e-refile-folder . "/work/Archive")
;;                   (mu4e-trash-folder . "/work/Deleted Messages")
;;                   (mu4e-maildir-shortcuts . (("/work/INBOX" . ?i)
;;                                              ("/work/Deleted Messages" . ?d)
;;                                              ("/work/Drafts" . ?D)
;;                                              ("/work/Sent Messages" . ?s)
;;                                              ("/work/Junk" . ?j)))
;;                   ))))
;;   ;; (mu4e t)
;;   )

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
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 用GUI tooltips来显示检查到的错误
;; (progn
;;   (use-package flycheck-posframe
;;                :custom-face (flycheck-posframe-border-face ((t
;;                                                               (:inherit default))))
;;                :hook (flycheck-mode . flycheck-posframe-mode)
;;                :init (setq flycheck-posframe-border-width 1 flycheck-posframe-inhibit-functions '((lambda
;;                                                                                                     (&rest _)
;;                                                                                                     (bound-and-true-p
;;                                                                                                       company-backend)))))
;;   (use-package flycheck-pos-tip
;;                :defines flycheck-pos-tip-timeout
;;                :hook (global-flycheck-mode . flycheck-pos-tip-mode)
;;                :config (setq flycheck-pos-tip-timeout 30))
;;   (use-package flycheck-popup-tip
;;                :hook (flycheck-mode . flycheck-popup-tip-mode)))

;;   ;; 浮动窗口支持
;;   (use-package posframe
;;                :custom
;;                (posframe-mouse-banish nil))

;; 彩虹猫进度条
;; (use-package nyan-mode
;;              :hook (after-init . nyan-mode)
;;              :config
;;              (setq nyan-wavy-trail t
;;                    nyan-animate-nyancat t))

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
;;       use-package-ensure-function 'quelpa)
;; (setq use-package-always-defer t)

;; for lisp
;; (use-package sly
;;   :hook (common-lisp-mode . sly-edit-mode))
;; (use-package sly-macrostep
;;   :hook (common-lisp-mode . sly-macrostep-mode))
;;
;; (use-package sly-repl-ansi-color
;;   :hook (common-lisp-mode . sly-repl-ansi-color))

;; for elisp
;; (use-package lispy
;;   :hook (emacs-lisp-mode . lispy-mode))

;; lisp符号操作工具
;; (use-package symbol-overlay
;;   :hook (emacs-lisp-mode . symbol-overlay-mode))

;;(use-package markdown-mode
;;  :mode "\\.md\\'"
;;  :mode "\\.text\\'"
;;  :mode "\\.markdown\\'")
;;(use-package markdown-mode+
;;  :defer t
;;  :hook (markdown-mode . (lambda () (require 'markdown-mode+)))
;;  :config
;;  ())

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
