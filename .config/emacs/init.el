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

;; emacs source code
;; (setq source-directory "/path/to/emacs/source/dir")
;; (setq source-directory
;;       (file-name-directory
;;        (shell-command-to-string
;;         (concat "locate --limit 1 emacs-" emacs-version "/src"))))
;; (setq find-function-C-source-directory (concat "~/src/emacs-" emacs-version "/src"))
;; ~/src/emacs-27.1/src
;; git clone https://github.com/emacs-mirror/emacs.git

;;----------------------------------------------------------------------------
;; init variables
;;----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

;; 关闭备份
(setq make-backup-files nil auto-save-default nil)
;; 关闭多编辑器同时编辑统一文件时锁文件操作
(setq create-lockfiles nil)
;; 随时重新加载发生修改过的文件
(setq load-prefer-newer t)
;; 关闭字体缓存gc
(setq inhibit-compacting-font-caches nil)
;; 关闭烦人的提示
(setq ring-bell-function 'ignore)

;; 关闭自动调节行高
(setq auto-window-vscroll nil)
;; 关闭自动换行的功能
(setq truncate-partial-width-windows nil)

;; 最大单行字符数量
(setq-default fill-column 80)
;; ;; 让'_'被视为单词的一部分
;; (add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;; ;; "-" 同上)
;; (add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w")))
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
(global-auto-revert-mode t)
(setq desktop-dirname  (expand-file-name "var/desktop-save" user-emacs-directory))
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix  (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))
;; minibuffer history
(setq savehist-file (expand-file-name "var/savehist" user-emacs-directory))
;; emacs bookmarks
(setq bookmark-default-file (expand-file-name "var/bookmarks" user-emacs-directory))

;; 更友好及平滑的滚动
(setq scroll-conservatively 100)
;;; scroll
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))

;; dired alternate open(avoid of too many buffers)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

;; join-line
(global-set-key "\M-J" (lambda ()
                         (interactive)
                         (next-line)
                         (join-line)
                         ))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "M-S-<up>") 'move-line-up)
(global-set-key (kbd "M-S-<down>") 'move-line-down)

;;  // -*- compile-command: "gcc -Wall -o test test.c && ./test" -*-
;; (require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
	        (unless (file-exists-p "Makefile")
	          (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		           (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s %s && ./%s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-O0")
                             (or (getenv "CFLAGS") "-Wall -Werror -Wextra -pedantic -g")  ;; (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			                 file
                             (file-name-sans-extension file)
                             ))))))
(add-hook 'go-mode-hook
          (lambda ()
	        (set (make-local-variable 'compile-command)
                 (format "go run %s" (file-name-nondirectory buffer-file-name))
                 )))
(add-hook 'python-mode-hook
          (lambda ()
	        (set (make-local-variable 'compile-command)
                 (format "python %s" (file-name-nondirectory buffer-file-name))
                 )))
(add-hook 'sh-mode-hook
          (lambda ()
	        (set (make-local-variable 'compile-command)
                 (format "sh %s" (file-name-nondirectory buffer-file-name))
                 )))
(add-hook 'java-mode-hook
          (lambda ()
	        (set (make-local-variable 'compile-command)
                 (format "javac %s && java %s" (file-name-nondirectory buffer-file-name) (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                 )))

                   
;;----------------------------------------------------------------------------
;; global common keybindings
;;----------------------------------------------------------------------------
;; some custom shortcut
;; (global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "C-z"))
(define-prefix-command 'leader-key)
;; (global-set-key (kbd "M-SPC") 'leader-key)
(global-set-key (kbd "C-\\") 'leader-key)
(define-key leader-key (kbd "<f5>") 'revert-buffer)
(define-key leader-key "fi" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key leader-key "fn" (lambda () (interactive) (find-file "~/doc/org/notes.org")))
(define-key leader-key "fp" (lambda () (interactive) (find-file "~/doc/org/personal.org")))
(define-key leader-key "fr" (lambda () (interactive) (find-file "~/doc/org/reading.org")))
(define-key leader-key "al" 'org-agenda-list)
(define-key leader-key "at" 'org-todo-list)
(global-set-key (kbd "M-L") 'org-agenda-list)
(global-set-key (kbd "M-T") 'org-todo-list)
;; (define-key leader-key "e" 'mu4e)

(defun my/org-narrow-forward ()
  "Move to the next subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (org-forward-heading-same-level 1)
  (org-narrow-to-subtree))
(defun my/org-narrow-backward ()
  "Move to the previous subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (org-backward-heading-same-level 1)
  (org-narrow-to-subtree))
(global-set-key (kbd "C-c n n") (lambda () (interactive) (my/org-narrow-forward)))
(global-set-key (kbd "C-c n p") (lambda () (interactive) (my/org-narrow-backward)))

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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; enable mouse in terminal
(xterm-mouse-mode 1)

;; line number column
;; (setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; show in mode line
;; show modeline column number
(column-number-mode 1)
;; Toggle buffer size display in the mode line (Size Indication mode).
(size-indication-mode 1)

;; 选中文本后输入会覆盖
(delete-selection-mode t)

;; setting initial frame size
(setq initial-frame-alist
       '((height . 55)
         (width . 160)
         (left . 10)
         (top . 40)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun my/duplicate-line ()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
(global-set-key (kbd "C-,") 'my/duplicate-line)

(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            (?\{ . ?\})
                            ))
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\<) t (electric-pair-default-inhibit c))
        ;; (if (char-equal c ?\") t (electric-pair-default-inhibit c))
        ))
(electric-pair-mode t)

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

;;;###autoload
;; Make frame transparency overridable
(defvar my/frame-transparency '(98 . 98))
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
(set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;;(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key (kbd "C-M-7") (lambda () (interactive) (my/toggle-transparency)))
(global-set-key (kbd "C-M-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "C-M-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "C-M-0") (lambda () (interactive) (my/toggle-proxy)))

;; Use region as the isearch text
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

(defun toggle-camelcase-underscores (first-lower-p)
  "Toggle between camelcase and underscore notation for the symbol at point.  FIRST-LOWER-P."
  (interactive "P")
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (replace-string "_" " " nil start end)
            (upcase-initials-region start end)
            (replace-string " " "" nil start end)
            (when first-lower-p
              (downcase-region start (1+ start))))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
(global-set-key (kbd "M-U") (lambda () (interactive) (toggle-camelcase-underscores nil)))

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

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Restore old window configurations
(winner-mode 1)

(use-package quickrun)

(use-package org-num
  :load-path "lisp/"
  :after org
  :hook (org-mode . org-num-mode))

(use-package windmove
  :config (use-package buffer-move)
  :bind (
         ("C-<left>" . windmove-left)
         ("C-<down>" . windmove-down)
         ("C-<up>" . windmove-up)
         ("C-<right>" . windmove-right)

         ("C-c C-<left>" . windmove-left)
         ("C-c C-<down>" . windmove-down)
         ("C-c C-<up>" . windmove-up)
         ("C-c C-<right>" . windmove-right)

         ("C-c S-<left>" . windmove-swap-states-left)
         ("C-c S-<down>" . windmove-swap-states-down)
         ("C-c S-<up>" . windmove-swap-states-up)
         ("C-c S-<right>" . windmove-swap-states-right)

         ("C-M-S-<left>" . shrink-window-horizontally)
         ("C-M-S-<down>" . enlarge-window)
         ("C-M-S-<up>" . shrink-window)
         ("C-M-S-<right>" . enlarge-window-horizontally)
         ))

;; ;; evil
;; (use-package evil
;;   ;; :disabled
;;   :init
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (eval-after-load "evil-maps"
;;     (dolist (map '(evil-motion-state-map
;;                    evil-normal-state-map
;;                    evil-operator-state-map
;;                    evil-replace-state-map
;;                    evil-insert-state-map
;;                    evil-visual-state-map
;;                    evil-emacs-state-map))
;;       (define-key (eval map) "\C-n" nil)
;;       (define-key (eval map) "\C-p" nil)
;;       (define-key (eval map) "\C-a" nil)
;;       (define-key (eval map) "\C-e" nil)
;;       (define-key (eval map) "\C-f" nil)
;;       (define-key (eval map) "\C-b" nil)
;;       (define-key (eval map) "\C-y" nil)
;;       (define-key (eval map) "\C-k" nil)
;;       (define-key (eval map) "\C-u" nil)
;;       (define-key (eval map) "\C-d" nil)
;;       ;; (define-key (eval map) "\C-z" nil)
;;       (define-key (eval map) "\M-." nil)
;;       ;; (define-key (eval map) "\C-w" nil)
;;       ;; (define-key (eval map) (kbd "SPC") nil)
;;       ;; (define-key (eval map) (kbd "RET") nil)
;;       (define-key (eval map) (kbd "TAB") nil) ;; let evil take care of tab and C-i translation
;;       )
;;     )
;;   (eval-after-load "evil-maps"
;;     (dolist (map '(
;;                    evil-insert-state-map
;;                    ))
;;       (define-key (eval map) "\C-i" nil)
;;       (define-key (eval map) "\C-o" nil)
;;       (define-key (eval map) "\C-w" nil)
;;       (define-key (eval map) "\C-v" nil)
;;       )
;;     )
;;   (setq evil-disable-insert-state-bindings t)
;;   (evil-mode 1))
;; 
;; (use-package evil-collection
;;   ;; :disabled
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; Use Ibuffer for Buffer List
(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        (quote (("home"
                 ("workspace" (filename . "workspace"))
                 ("dotfiles" (or (filename . "dotfiles")))
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

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  ;; Recommended keymap prefix on macOS
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; extra search utilities
(define-prefix-command 'meta-s-prefix)
(global-set-key (kbd "M-s") 'meta-s-prefix)

(defun my/avy-goto-end-of-line ()
  "Call avy-goto-char for RET(13)."
  (interactive)
  (avy-goto-char 13))

(use-package avy
  :ensure t
  :bind (
         :map meta-s-prefix
         ("c" . avy-goto-char)
         ("C" . avy-goto-char-2)
         ("<RET>" . my/avy-goto-end-of-line)
         ))

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'fzf-recentf)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; fzf
(use-package fzf
  :bind (("M-O" . fzf)))

;; rg
(use-package rg)

;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult start

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(setq prefix-help-command 'embark-prefix-help-command)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-ripgrep-args "rg --null --column --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden -g \"!.git\" .")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  (setq consult-async-min-input 2) ;; Minimum number of letters needed, before asynchronous process is called.
)

;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult end

;; ;; ---------------------------------------------------------------------------- ido and smex

;; (ido-mode 1)
;; (setq ido-enable-flex-matching 1)
;; (setq ido-create-new-buffer 'always)
;; (ido-everywhere 1)
;; (global-set-key (kbd "C-M-j") 'ido-switch-buffer)

;; (setq magit-completing-read-function 'magit-ido-completing-read)
;; (setq gnus-completing-read-function 'gnus-ido-completing-read)

;; ;; built-in, ido-like behavior
;; (require 'icomplete)
;; (icomplete-mode 1)

;; (use-package ido-completing-read+
;;   :config (ido-ubiquitous-mode 1))

;; (use-package smex
;;   :bind (
;;          ("M-x" . 'smex)
;;          ("M-X" . 'smex-major-mode-commands)
;;          ("C-c C-c M-x" . 'execute-extended-command)
;;          )
;;   )

;; ;; ---------------------------------------------------------------------------- ido and smex

;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper start
;; other similar packages to prescient: vertico consult selectrum prescient for completion

;; (use-package swiper
;;   :ensure t
;;   :bind ("C-s" . 'swiper))

;; (use-package counsel
;;   :bind (
;;          ("M-x" . 'counsel-M-x)
;;          ("C-x b" . 'counsel-ibuffer)
;;          ("C-M-j" . 'counsel-switch-buffer) ;; skip corrent buffer, more efficient
;;          ("C-x d" . 'counsel-dired)
;;          ("C-x C-f" . 'counsel-find-file)
;;          ("C-x C-r" . 'counsel-recentf)
;;          ("C-c C-r" . 'ivy-resume)
;;          ("<f1> f" . 'counsel-describe-function)
;;          ("<f1> v" . 'counsel-describe-variable)
;;          ("<f1> o" . 'counsel-describe-symbol)
;;          ("<f1> l" . 'counsel-find-library)
;;          ("<f2> i" . 'counsel-info-lookup-symbol)
;;          ("<f2> u" . 'counsel-unicode-char)
;;          ("C-c g" . 'counsel-git)
;;          ("C-c j" . 'counsel-git-grep)
;;          ;; ("C-x l" . 'counsel-locate)
;;          ;; ("C-S-o" . 'counsel-rhythmbox)
;;          :map meta-s-prefix
;;          ("g" . #'counsel-rg)
;;          ("a" . #'counsel-ag)
;;          ("f" . #'counsel-fzf)
;;          ("G" . #'counsel-git)
;;          ("F" . #'counsel-git-grep)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history)
;;          )
;;   :config
;;   ;; 默认的 rg 配置
;;   ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
;;   (setq counsel-rg-base-command "rg -i -M 240 --with-filename --no-heading --line-number --color never %s -g !doc -g !themes -g !quelpa")
;;   (setq counsel-fzf-cmd "fd -I --exclude={site-lisp,etc/snippets,themes,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/} --type f | fzf -f \"%s\" --algo=v1")
;;   ;; Integration with 'projectile'
;;   (with-eval-after-load 'projectile
;;     (setq projectile-completion-system 'ivy)))

;; (use-package ivy
;;   :diminish ivy-mode
;;   :config
;;   ;; (setq ivy-re-builders-alist
;;   ;;     '((swiper . ivy--regex-fuzzy)
;;   ;;       (t      . ivy--regex-plus))) ;; use ivy--regex-plus(default) for the rest of components
;;   (use-package ivy-rich
;;     :config
;;     (ivy-rich-mode 1))
;;   (use-package ivy-prescient
;;     :after counsel
;;     :config
;;     (setq prescient-sort-length-enable nil)
;;     ;; This is the default value!
;;     (setq prescient-filter-method '(literal regexp fuzzy))
;;     ;; If you are too used to Ivy’s filtering styles, you can use those while still keeping Prescient’s sorting:
;;     (setq ivy-prescient-enable-filtering nil)
;;     ;; Getting the old highlighting back
;;     ;; (setq ivy-prescient-retain-classic-highlighting t)
;;     (ivy-prescient-mode 1)
;;     ;; Remember candidate frequencies across sessions
;;     (prescient-persist-mode 1))
;;   )

;; ;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper end

;; (use-package prescient
;;   :after counsel
;;   :config
;;   (prescient-persist-mode 1))

;; (use-package company-prescient
;;   :after company
;;   :config
;;   (company-prescient-mode 1))

;; (use-package sudo-edit)

;; ;; try
;; (use-package try)

;; (use-package figlet
;;   :config
;;   (setq figlet-default-font "standard"))

;; (use-package editorconfig
;;   :ensure t
;;   :config
;;   (editorconfig-mode 1))

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
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/etc/snippets"))
  (yas-global-mode))

(use-package yasnippet-snippets :after yasnippet)

(setq kill-ring-max 500)
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

(require 'url-util)
(use-package ob-go)

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

;;; org
;;(image-type-available-p 'imagemagick) ;; It will evaluate to t if your Emacs has Imagemagick support.
(defun ndk/org-display-inline-image-at-point ()
  "Toggle inline image at point."
  (interactive)
  (let* ((context (org-element-context (org-element-at-point)))
         (type (org-element-type context))
         (beg  (plist-get (cadr context) :begin))
         (end  (plist-get (cadr context) :end)))
    (when (eq type 'link)
      (org-display-inline-images nil nil beg end))))

(use-package org
  :diminish org-indent-mode
  :config
  ;; This can be solved by adding a hook to org-tab-first-hook which adds org-end-of-line.
  ;; Every time TAB is used it jumps to last visible character of the org-line, but before the ellipsis, and then opens/closes the container as usual.
  (add-hook 'org-tab-first-hook 'org-end-of-line)
  ;; (setq org-hide-emphasis-markers t) ;; hide markers like *bold* or /italic/
  (setq org-ellipsis "▾")
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 200)
  (eval-after-load 'org
    (progn
      (define-key org-mode-map (kbd "<C-M-S-right>") nil)
      (define-key org-mode-map (kbd "<C-M-S-left>") nil)
      (define-key org-mode-map (kbd "C-,") nil)
      (define-key org-mode-map (kbd "C-c C-v") 'ndk/org-display-inline-image-at-point)
      ))
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
                        "~/doc/org/reading.org"
                        ))
(add-hook 'org-mode-hook 'org-indent-mode)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Type C-h v org-agenda-window-setup for other options.
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-restore-windows-after-quit t)

(setq org-directory "~/doc/org/")
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/doc/org/notes.org" "Tasks")
         ;; Prompt for tag
         "* TODO %?\t%^g\n Entered on %U\n  %i\n  %a")
        ("o" "Someday" entry (file+headline "~/doc/org/personal.org" "Tasks")
         "* SOMEDAY %?")
        ("s" "Code Snippet" entry (file+datetree "~/doc/org/snippet.org")
         ;; Prompt for tag and language
         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ))

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

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  ;; :custom (org-bullets-bullet-list '("☰" "☷" "✿" "☭"))
  )

(use-package flycheck
  :commands (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :bind (:map leader-key
              ("t t" . global-flycheck-mode))
  :config
  (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode org-mode diff-mode
                                    shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled) flycheck-indication-mode
        'right-fringe)
  )

;; 回到关闭文件前光标的位置
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "var/places" "var/.emacs-places"))
  :hook (after-init . (lambda () (save-place-mode t))))

(use-package which-key
  :diminish which-key-mode
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode 1))

;; magit
(use-package magit
  :commands (magit)
  ;; :bind (("C-x C-g" . magit-status))
  )

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :custom
  (git-gutter:update-interval 0)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:modified-sign "~")
  (git-gutter:hide-gutter t))

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

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package diminish
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

(use-package posframe :defer t)

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :config (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-c y" "有道翻译")
  :bind (("C-c y y" . 'youdao-dictionary-search-at-point+)
         ("C-c y g" . 'youdao-dictionary-search-at-point-posframe)
         ("C-c y p" . 'youdao-dictionary-play-voice-at-point)
         ("C-c y r" . 'youdao-dictionary-search-and-replace)
         ("C-c y i" . 'youdao-dictionary-search-from-input)))

(progn
  (use-package all-the-icons)
  (use-package all-the-icons-dired
    :hook ('dired-mode . 'all-the-icons-dired-mode))
  (use-package emojify
    :custom (emojify-emojis-dir (expand-file-name "var/emojis" user-emacs-directory))))

;; C-x u runs the command undo-tree-visualize (found in undo-tree-map)
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t undo-tree-enable-undo-in-region nil undo-tree-auto-save-history nil)
  ;; HACK: keep the diff window
  (with-no-warnings (make-variable-buffer-local 'undo-tree-visualizer-diff)
                    (setq-default undo-tree-visualizer-diff t)))

;; ;; Failed to verify signature queue-0.2.el.sig: No public key for 066DAFCB81E42C40 created at 2019-09-22T01:54:25+0800 using RSA
;; (setq package-check-signature nil)

(use-package zenburn-theme
  :config
  (setq zenburn-override-colors-alist
        '(
          ("zenburn-bg" . "#292929") ;; background
          ("zenburn-bg+1" . "#292929") ;; fringe
          ("zenburn-bg-1" . "#404040") ;; region highlight
          ("zenburn-bg-05" . "#292929") ;; line number
          ;; ("zenburn-bg-05" . "#2e2e2e") ;; line number
          ))
  (load-theme 'zenburn t))

(use-package yaml-mode :defer t)
(use-package json-mode
  :mode "\\.json\\'")

;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))

;; (use-package dockerfile-mode
;;   :defer t)

;; (use-package hackernews
;;   :commands (hackernews)
;;   :bind
;;   ("C-c h" . hackernews)
;;   )

;; ;; golang
(use-package go-mode)
;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-save-hooks ()
  "Go file before save, format and organize imports."
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t) ;; don't auto format, in case of lsp remove unsed import before tmp save
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-save-hooks)

;; Start LSP Mode and YASnippet mode
;; (add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook 'lsp)

;; ;; python  use lsp-pyright to setup lsp instead
;; (add-hook 'python-mode-hook 'lsp)

;; ;; bash
(add-hook 'shell-mode-hook 'lsp)
(add-hook 'sh-mode-hook 'lsp)

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
;; =   > `(use-package lsp-java)'                  =
;; = + c/c++                                       =
;; =   > `sudo pacman -S ccls'                     =
;; = + go                                          =
;; =   > `sudo pacman -S gopls'                    =
;; =================================================
;; The palantir python-language-server (pyls) is unmaintained;
;; a maintained fork is the python-lsp-server (pylsp) project;
;; you can install it with pip via: pip install python-lsp-server
(use-package lsp-mode
  :defer t
  :commands lsp
  :hook (
         ((
           python-mode go-mode rust-mode
           js-mode js2-mode typescript-mode web-mode
           c-mode c++-mode objc-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :bind (:map lsp-mode-map
              ;; ("C-M-l" . lsp-format-buffer)
              ("C-M-l" . lsp-format-region)
              ("M-RET" . lsp-ui-sideline-apply-code-actions)
              ("M-RET" . lsp-execute-code-action))
  :config
  (setq lsp-completion-enable-additional-text-edit nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil)
  )

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
  ;; (if (display-graphic-p)
  ;;     (setq lsp-ui-doc-enable t)
  ;;   (setq lsp-ui-doc-enable nil)
  ;;   )
  ;; (setq lsp-ui-doc-position 'at-point        ;; 文档显示的位置
  ;;       lsp-ui-doc-delay 0.1        ;; 显示文档的延迟
  ;;       )
  )

;; C/C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(define-key c-mode-map (kbd "C-c C-\\") nil) ;; use it for toggle-input-method, instead of c-backslash-region
;; (eval-after-load "c"
;;   '(progn (define-key c-mode-map (kbd "C-c C-\\") nil)
;;           ))
;; (setq c-mode-hook
;;       '(lambda ()
;;          (gtags-mode 1)
;;          ))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; (use-package lsp-java
;;   :after lsp-mode
;;   :if (executable-find "mvn")
;;   :init
;;   (use-package request :defer t)
;;   ;; :custom
;;   ;; lsp-install-server --> jdtls
;;   ;; (lsp-java-server-install-dir (expand-file-name "~/.config/emacs/eclipse.jdt.ls/server/"))
;;   ;; (lsp-java-workspace-dir (expand-file-name "~/.config/emacs/eclipse.jdt.ls/workspace/"))
;;   )

;; ;; pdf
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   )

;; (defun my/scroll-other-window ()
;;   "Scroll ather window up."
;;   (interactive)
;;   (let* ((wind (other-window-for-scrolling))
;;          (mode (with-selected-window wind major-mode)))
;;     (if (eq mode 'pdf-view-mode)
;;         (with-selected-window wind
;;           ;; (pdf-view-scroll-up-or-next-page &optional ARG)
;;           ;; (pdf-view-scroll-up-or-next-page)
;;           (pdf-view-next-line-or-next-page 2)
;;           )
;;       ;; (scroll-other-window 2)
;;       (scroll-other-window)
;;       )))

;; (defun my/scroll-other-window-down ()
;;   "Scroll ather window down."
;;   (interactive)
;;   (let* ((wind (other-window-for-scrolling))
;;          (mode (with-selected-window wind major-mode)))
;;     (if (eq mode 'pdf-view-mode)
;;         (with-selected-window wind
;;           (progn
;;             ;; (pdf-view-scroll-down-or-previous-page &optional ARG)
;;             ;; (pdf-view-scroll-down-or-previous-page)
;;             (pdf-view-previous-line-or-previous-page 2)
;;             (other-window 1)))
;;       ;; (scroll-other-window-down 2)
;;       (scroll-other-window-down)
;;       )))

;; (use-package org-pdftools
;;   :hook (org-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freestyle-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))
;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((inhibit-read-only t)
;;            (ast (org-noter--parse-root))
;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;        (with-current-buffer (org-noter--session-notes-buffer session)
;;          (org-with-wide-buffer
;;           (goto-char (org-element-property :begin ast))
;;           (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;             (org-entry-put nil org-noter-property-note-location
;;                            (org-noter--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; (use-package org-noter
;;   :config
;;   ;; Your org-noter config
;;   (require 'org-noter-pdftools))

;; ;; input method
;; (use-package pyim
;;   :init
;;   (use-package posframe :defer t)
;;   :diminish pyim-isearch-mode
;;   :custom
;;   (default-input-method "pyim")
;;   (pyim-default-scheme 'xiaohe-shuangpin)
;;   (pyim-page-tooltip 'posframe)
;;   (pyim-page-length 9)
;;   :bind ("C-c C-\\" . toggle-input-method)
;;   :config
;;   (use-package pyim-basedict
;;     :after pyim
;;     :config (pyim-basedict-enable))
;;   (pyim-isearch-mode 1)
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-isearch-mode
;;                   pyim-probe-org-structure-template))
;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))
;;   ;; :bind ("M-j" . pyim-convert-string-at-point) ; M-j 强制将光标前的拼音字符串转换为中文
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

;; init message
;; (setq-default initial-scratch-message
;;               (concat ";; Hi, " user-login-name ", Emacs ♥ you!\n\n"))

;; 设置光标样式
(setq-default cursor-type t)
(blink-cursor-mode 1)

;; 高亮当前行
;; (global-hl-line-mode 1)

;; Move cursor to end of current line
;; Insert new line below current line
;; it will also indent newline
(global-set-key (kbd "<S-return>") (lambda ()
                                     (interactive)
                                     (end-of-line)
                                     (newline-and-indent)))
;; Move cursor to previous line
;; Go to end of the line
;; Insert new line below current line (So it actually insert new line above with indentation)
;; it will also indent newline
(global-set-key (kbd "<C-M-return>") (lambda ()
                                       (interactive)
                                       (previous-line)
                                       (end-of-line)
                                       (newline-and-indent)
                                       ))

;; Window size and features
;; (setq-default
;;  window-resize-pixelwise t
;;  frame-resize-pixelwise t)

;; fringe setup (clear line wrap symbol)
;; (setf (cdr (assq 'continuation fringe-indicator-alist))
;;       '(nil nil) ;; no continuation indicators
;;       ;; '(nil right-curly-arrow) ;; right indicator only
;;       ;; '(left-curly-arrow nil) ;; left indicator only
;;       ;; '(left-curly-arrow right-curly-arrow) ;; default
;;       )
;; disable fringe column
;; (fringe-mode '(0 . 0))

;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (setq line-spacing 0)))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-cocoa-emacs* (and *is-mac* (eq window-system 'ns)))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-x11* (eq window-system 'x))
(defconst *is-windows* (eq system-type 'windows-nt))
(when *is-mac*
  (setq mac-command-modifier 'meta) ;; use command key as meta/alt
  ;; (setq mac-option-modifier 'none) ;; If ‘none’, the key is ignored by Emacs and retains its standard meaning.
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  ;; (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  ;; (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  ;; (with-eval-after-load 'nxml-mode
  ;;   (define-key nxml-mode-map (kbd "M-h") nil))
  ;; ;; what describe-key reports for cmd-option-h
  ;; (global-set-key (kbd "M-ˍ") 'ns-do-hide-others)
  )

;; (when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
;;   ;; Command-Option-f to toggle fullscreen mode
;;   ;; Hint: Customize `ns-use-native-fullscreen'
;;   (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; 设置字体
;; (when *is-linux*
;;   ;; "Fira Code Nerd Font Mono"
;;   (set-face-attribute 'default nil
;;                       :font (font-spec
;;                              ;; :name "Iosevka"
;;                              ;; :style "Regular"
;;                              :size 15))
;;   )
;; (when *is-mac*
;;   (set-face-attribute 'default nil
;;                       :font (font-spec
;;                              ;; :style "Regular"
;;                              :size 12))
;;   )
(set-face-attribute 'default nil
                    :font (font-spec
                           :name "JetbrainsMono Nerd Font"
                           :size 12))


;; (when *is-windows* (set-next-selection-coding-system 'utf-16-le)  (set-selection-coding-system 'utf-16-le)  (set-clipboard-coding-system 'utf-16-le))

;; 任何地方都使用UTF-8
;; (set-charset-priority 'unicode)
;; (setq locale-coding-system   'utf-8)
;; (set-terminal-coding-system  'utf-8)
;; (set-keyboard-coding-system  'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system        'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8))

;; (use-package centaur-tabs
;;   :demand
;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)
;;   :config
;;   (setq
;;    centaur-tabs-style "bar"
;;    centaur-tabs-set-bar 'over
;;    centaur-tabs-set-icons t
;;    centaur-tabs-set-modified-marker t
;;    centaur-tabs-modified-marker "*"
;;    ;; centaur-tabs-height 32
;;    ;; centaur-tabs-plain-icons t
;;    ;; centaur-tabs-gray-out-icons 'buffer
;;    ;; centaur-tabs--buffer-show-groups t
;;    ;; centaur-tabs-show-navigation-buttons t
;;    )
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-mode t)
;;   :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   ;; :bind (
;;   ;;        ("C-<prior>" . centaur-tabs-backward)
;;   ;;        ("C-<next>" . centaur-tabs-forward)
;;   ;;        ;; :map evil-normal-state-map
;;   ;;        ;; ("g t" . centaur-tabs-forward)
;;   ;;        ;; ("g T" . centaur-tabs-backward)
;;   ;;        )
;;   )

;; (use-package command-log-mode
;;   :ensure t)

;; ;; for test
;; (defun my/check-cursor-end-of-line ()
;;   "Check end of line."
;;   (interactive)
;;   (if
;;       (eq ?\n (char-after))
;;       (message "yes")
;;     (message "no")
;;     )
;;   )

;; REST
;; (use-package restclient
;;   :mode ("\\.http\\'" . restclient-mode)
;;   :config
;;   (use-package ob-restclient)
;;   (use-package restclient-test
;;     :diminish
;;     :hook (restclient-mode . restclient-test-mode))
;;   (with-eval-after-load 'company
;;     (use-package company-restclient
;;       :defines company-backends
;;       :init (add-to-list 'company-backends 'company-restclient))))
;; (evil-leader/set-key-for-mode 'restclient-mode
;;                               "mn" 'restclient-jump-next
;;                               "mp" 'restclient-jump-prev
;;                               "ms" 'restclient-http-send-current-stay-in-window
;;                               "mS" 'restclient-http-send-current
;;                               "mr" 'spacemacs/restclient-http-send-current-raw-stay-in-window
;;                               "mR" 'restclient-http-send-current-raw
;;                               "my" 'restclient-copy-curl-command)

;; 切换buffer焦点时高亮动画
;; (use-package beacon
;;   :hook (after-init . beacon-mode))

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

(use-package doom-modeline
  :init (doom-modeline-mode 1))

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

;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode))

;; 如果不喜欢ivy可以用这个包替换
;; (use-package selectrum :config (selectrum-mode t))

;; (use-package selectrum-prescient
;;     :config
;;     (prescient-persist-mode t)
;;     (selectrum-prescient-mode t))

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
;;                         (hydra-posframe-mode t) ) ))

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
