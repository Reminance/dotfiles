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

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq machine-specific-file (expand-file-name "machine-specific.el" user-emacs-directory))
(when (file-exists-p machine-specific-file)
  (load machine-specific-file))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'special-mode-hook 'remove-dos-eol)  ;;  *LLDB::Run out*      buffer is special-mode, to get rid of ^M character

;; init message
;; (setq-default initial-scratch-message
;;               (concat ";; Hi, " user-login-name ", Emacs ♥ you!\n\n"))

;; 设置光标样式
(setq-default cursor-type t)
(blink-cursor-mode 1)

;; 补全忽略大小写
(setq completion-ignore-case t)

;; 高亮当前行
;; (global-hl-line-mode 1)

;; hide continuation indicators ;; 隐藏连续行指示器
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

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
                                       (beginning-of-line)
                                       (newline-and-indent)
                                       (previous-line)
                                       (indent-for-tab-command)))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-x11* (eq window-system 'x))
(defconst *is-windows* (eq system-type 'windows-nt))
(when *is-mac*
  (pixel-scroll-mode)
  (setq mac-command-modifier 'meta) ;; use command key as meta/alt
  ;; (setq mac-option-modifier 'none) ;; If ‘none’, the key is ignored by Emacs and retains its standard meaning.
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
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
(setq-default show-trailing-whitespace t)
;; disable `show-trailing-whitespace` in unpropriate place
(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq-local show-trailing-whitespace nil))))

;; 高亮对应的括号
(show-paren-mode 1)

;; 设置eshell历史记录
(setq url-configuration-directory (expand-file-name "var/url" user-emacs-directory))
(setq eshell-history-file-name (expand-file-name "var/eshell/history" user-emacs-directory))
(setq recentf-save-file (expand-file-name "var/recentf" user-emacs-directory))
;; 自动刷新被修改过的文件
(global-auto-revert-mode t)
(setq desktop-dirname (expand-file-name "var/desktop-save" user-emacs-directory))
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))
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
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
            ))

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
(global-set-key (kbd "C-S-<up>") 'move-line-up)
(global-set-key (kbd "C-S-<down>") 'move-line-down)

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
                 (format "javac %s && java %s"
                         (file-name-nondirectory buffer-file-name)
                         (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                 )))

;;----------------------------------------------------------------------------
;; global common keybindings
;;----------------------------------------------------------------------------
;; some custom shortcut
;; (global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "C-z"))
(define-prefix-command 'leader-key)

;; M-s -- extra search utilities
(global-set-key (kbd "M-s") 'meta-s-prefix)
(define-prefix-command 'meta-s-prefix)

;; (global-set-key (kbd "M-SPC") 'leader-key)
(global-set-key (kbd "C-\\") 'leader-key)
(define-key leader-key (kbd "<f5>") 'revert-buffer)
;; (define-key leader-key "e" 'mu4e)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; (global-set-key (kbd "C-x C-r") 'fzf-recentf)

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
         (width . 200)
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
(defvar my/frame-transparency '(96 . 96))
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

(global-set-key (kbd "C-M-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "C-M-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "C-M-0") (lambda () (interactive) (my/toggle-transparency)))
;;(global-set-key (kbd "C-M-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

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

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; org-mode
;; ----------------------------------------------------------------------------
(define-key leader-key "fi" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key leader-key "fn" (lambda () (interactive) (find-file "~/doc/org/notes.org")))
(define-key leader-key "fp" (lambda () (interactive) (find-file "~/doc/org/personal.org")))
(define-key leader-key "fr" (lambda () (interactive) (find-file "~/doc/org/reading.org")))
(define-key leader-key "al" 'org-agenda-list)
(define-key leader-key "at" 'org-todo-list)
(global-set-key (kbd "M-L") 'org-agenda-list)
(global-set-key (kbd "M-T") 'org-todo-list)

(with-eval-after-load 'org
  ;; This can be solved by adding a hook to org-tab-first-hook which adds org-end-of-line.
  ;; Every time TAB is used it jumps to last visible character of the org-line, but before the ellipsis, and then opens/closes the container as usual.
  (add-hook 'org-tab-first-hook 'org-end-of-line)
  ;; (setq org-hide-emphasis-markers t) ;; hide markers like *bold* or /italic/
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 200)
  ;; (setq org-ellipsis "▾") ;; todo. not working
  ;; (setq org-ellipsis "↴") ;; todo. not working
  (setq org-ellipsis "⤵") ;; todo. not working
  (define-key org-mode-map (kbd "<C-M-S-right>") nil)
  (define-key org-mode-map (kbd "<C-M-S-left>") nil)
  (define-key org-mode-map (kbd "<C-S-up>") nil)
  (define-key org-mode-map (kbd "<C-S-down>") nil)
  (define-key org-mode-map (kbd "C-,") nil)
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
(global-set-key (kbd "C-c o n") (lambda () (interactive) (my/org-narrow-forward)))
(global-set-key (kbd "C-c o p") (lambda () (interactive) (my/org-narrow-backward)))
(global-set-key (kbd "C-c o SPC") 'org-toggle-narrow-to-subtree)

;;----------------------------------------------------------------------------
;; package
;; ----------------------------------------------------------------------------
(require 'package)
;; (require 'cl-lib)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; ;; Standard package repositories
;; ;; (add-to-list 'package-archives '( "melpa" . "http://melpa.org/packages/") t)
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

;;----------------------------------------------------------------------------
;; custom package
;;----------------------------------------------------------------------------
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

(use-package quickrun)

(use-package org-num
  :load-path "lisp/"
  :after org
  :hook (org-mode . org-num-mode))

;; Restore old window configurations
;; (winner-mode 1)

(use-package windmove
  :bind (
         ("C-M-<left>" . windmove-left)
         ("C-M-<down>" . windmove-down)
         ("C-M-<up>" . windmove-up)
         ("C-M-<right>" . windmove-right)

         ("C-c S-<left>" . windmove-swap-states-left)
         ("C-c S-<down>" . windmove-swap-states-down)
         ("C-c S-<up>" . windmove-swap-states-up)
         ("C-c S-<right>" . windmove-swap-states-right)

         ("C-M-S-<left>" . shrink-window-horizontally)
         ("C-M-S-<down>" . enlarge-window)
         ("C-M-S-<up>" . shrink-window)
         ("C-M-S-<right>" . enlarge-window-horizontally)
         ))

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

;; rg
(use-package rg)

(use-package multiple-cursors
  :bind (
         ("C-c M-N" . mc/edit-lines)
         ("M-N" . mc/mark-next-like-this)
         ("M-P" . mc/unmark-next-like-this)
         ;; ("M-P" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
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

(use-package yaml-mode :defer t)
(use-package json-mode
  :mode "\\.json\\'")

(progn
  (use-package all-the-icons)
  (use-package all-the-icons-dired
    :hook ('dired-mode . 'all-the-icons-dired-mode))
  (use-package emojify
    :custom (emojify-emojis-dir (expand-file-name "var/emojis" user-emacs-directory))))

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

;; (use-package spacemacs-theme
;;   :config
;;   (load-theme 'spacemacs-theme t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :config (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-c y" "有道翻译")
  :bind (("C-c y y" . 'youdao-dictionary-search-at-point+)
         ("C-c y g" . 'youdao-dictionary-search-at-point-posframe)
         ("C-c y p" . 'youdao-dictionary-play-voice-at-point)
         ("C-c y r" . 'youdao-dictionary-search-and-replace)
         ("C-c y i" . 'youdao-dictionary-search-from-input)))

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
           python-mode go-mode c-mode c++-mode) . lsp-deferred)
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
;; (define-key c-mode-map (kbd "C-c C-\\") nil) ;; use it for toggle-input-method, instead of c-backslash-region
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

;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper start
;; other similar packages to prescient: vertico consult selectrum prescient for completion

(use-package swiper
  :ensure t
  :bind ("C-S-s" . 'swiper)
  )

(use-package counsel
  :bind (
         ;; ("M-x" . 'counsel-M-x)
         ;; ("C-x b" . 'counsel-ibuffer)
         ("C-M-j" . 'counsel-switch-buffer) ;; skip corrent buffer, more efficient
         ("C-x d" . 'counsel-dired)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x C-r" . 'counsel-recentf)
         ("C-c C-r" . 'ivy-resume)
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         :map meta-s-prefix
         ("g" . #'counsel-rg)
         ("a" . #'counsel-ag)
         ("f" . #'counsel-fzf)
         ("n" . #'counsel-git)
         ("G" . #'counsel-git-grep)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
  :config
  ;; 默认的 rg 配置
  (setq counsel-rg-base-command "rg --hidden --ignore-case --max-columns 240 --with-filename --no-heading --line-number --color never %s -g \"!.git\" -g !doc -g !themes -g !quelpa -g !backup ")
  (setq counsel-fzf-cmd "fd --hidden --exclude={.git,site-lisp,etc/snippets,themes,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/,backup} --type f | fzf -f \"%s\" --algo=v1")
  ;; Integration with 'projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (use-package ivy-rich
    :config
    (ivy-rich-mode 1))
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
  )

;; 各个语言的Debug工具
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (dap-mode 1)
  ;; ;; The modes below are optional
  ;; (dap-ui-mode 1)
  ;; ;; enables mouse hover support
  ;; (dap-tooltip-mode 1)
  ;; ;; use tooltips for mouse hover
  ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
  ;; (tooltip-mode 1)
  ;; ;; displays floating panel with debug buttons
  ;; ;; requies emacs 26+
  ;; (dap-ui-controls-mode 1)

  (require 'dap-python)
  (require 'dap-lldb)
  (require 'dap-dlv-go)

  ;; ;; set the debugger executable (c++)
  ;; ;; (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
  (setq dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-vscode"))

  ;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
  )

;; ;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper end


;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult start

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; A few more useful configurations...
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(setq prefix-help-command 'embark-prefix-help-command)

;; Example configuration for Consult
(use-package consult
  ;; ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;;        ("C-c h" . consult-history)
         ;;        ("C-c m" . consult-mode-command)
         ;;        ("C-c k" . consult-kmacro)
         ;;        ;; C-x bindings (ctl-x-map)
         ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;;        ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
         ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;;        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;;        ;; Custom M-# bindings for fast register access
         ;;        ("M-#" . consult-register-load)
         ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;        ("C-M-#" . consult-register)
         ;;        ;; Other custom bindings
         ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;;        ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;;        ;; M-g bindings (goto-map)
         ;;        ("M-g e" . consult-compile-error)
         ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;;        ("M-g m" . consult-mark)
         ;;        ("M-g k" . consult-global-mark)
         ;;        ("M-g i" . consult-imenu)
         ;;        ("M-g I" . consult-imenu-multi)
         ;;        ;; M-s bindings (search-map)
         ;;        ("M-s d" . consult-find)
         ;;        ("M-s D" . consult-locate)
         ;;        ("M-s g" . consult-grep)
         ;;        ("M-s G" . consult-git-grep)
         ;;        ("M-s r" . consult-ripgrep)
         ;;        ("M-s l" . consult-line)
         ;;        ("M-s L" . consult-line-multi)
         ;;        ("M-s m" . consult-multi-occur)
         ;;        ("M-s k" . consult-keep-lines)
         ;;        ("M-s u" . consult-focus-lines)
         ;;        ;; Isearch integration
         ;;        ("M-s e" . consult-isearch-history)
         ;;        :map isearch-mode-map
         ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;;        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;;        ;; Minibuffer history
         ;;        :map minibuffer-local-map
         ;;        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;;        ("M-r" . consult-history)                ;; orig. previous-matching-history-element
         )

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-ripgrep-args "rg --null --column --line-buffered --color=never --max-columns=240 --ignore-case --no-heading --line-number --hidden -g \"!.git\" -g !themes -g !quelpa -g !backup .")
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-async-min-input 2) ;; Minimum number of letters needed, before asynchronous process is called.
  )

;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult end

(use-package ctable)

(use-package request)

;; ;; ---------------------------------------------------------------------------- dbadmin start

(defvar dbadmin-cookie "")
(defvar dbadmin-database '(("dbId" . "199") ("dbName" . "wms_warehouse_within")))
(defvar dbadmin-page-no 1)
(defvar dbadmin-page-size 1000)
(defvar dbadmin-buffer-name "*dbadmin*")
(defvar dbadmin-database-alist '(
                                 (?1 "wws" (lambda () (message "Choosen: wws") (setq dbadmin-database '(("dbId" . "199") ("dbName" . "wms_warehouse_within")))))
                                 (?2 "wss" (lambda () (message "Choosen: wss") (setq dbadmin-database '(("dbId" . "104") ("dbName" . "newsitetest")))))
                                 (?3 "wms" (lambda () (message "Choosen: wms") (setq dbadmin-database '(("dbId" . "25") ("dbName" . "newsitetest")))))
                                 (?4 "stat" (lambda () (message "Choosen: stat") (setq dbadmin-database '(("dbId" . "147") ("dbName" . "wms_stat")))))
                                 (?5 "archive_1" (lambda () (message "Choosen: archive_1") (setq dbadmin-database '(("dbId" . "90") ("dbName" . "wms_archiver")))))
                                 ))

(defun dbadmin-choose-database ()
  "Dbadmin choose database."
  (interactive)
  (let ((choice (read-char-choice (mapconcat (lambda (item) (format "%c: %s" (car item) (cadr item))) dbadmin-database-alist "; ")
                  (mapcar #'car dbadmin-database-alist))))
    (funcall (nth 2 (assoc choice dbadmin-database-alist)))))

(defun dbadmin-set-page-size ()
  "Please enter dbadmin page size."
  (interactive)
  (let* (
         (page-size (read-number "Please enter dbadmin page size:"))
         )
    (setq dbadmin-page-size page-size)
    (message "Successfully set dbadmin-page-size: %s" page-size))
  )

(defun dbadmin-set-cookie ()
  "Please enter dbadmin cookie."
  (interactive)
  (let* (
         (cookie-input (read-string "Dbadmin cookie:"))
         )
    (setq dbadmin-cookie cookie-input)
    (message "Successfully set dbadmin-cookie: %s" cookie-input))
  )

(defun decode-hex-string (hex-string)
  "How do I convert a HEX-STRING into ASCII using elisp?  https://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp."
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

(defun dbadmin-check-buffer-exist()
  "Dbadmin check buffer exist, get or create it."
  (interactive)
  (cond ((eq (get-buffer dbadmin-buffer-name) (window-buffer (selected-window))) ;; (message "Visible and focused")
         )
        ((get-buffer-window (get-buffer dbadmin-buffer-name)) ;; (message "Visible and unfocused")
         )
        (t
         (split-window-below)
         (windmove-down)
         (switch-to-buffer dbadmin-buffer-name) ;; (message "Not visible")
         ))
  )

(defun dbadmin-exec (operation)
  "Exec OPERATION via dbadmin http request."
  (interactive)
  (if (and (string= (buffer-substring-no-properties (region-beginning) (region-end)) "")
           (or (string= operation "executeSql") (string= operation "explainSql") (string= operation "showTableStruct")))
      (user-error "Error, because: %s" "sql cannot be null")
    nil)
  (save-excursion
    (let* (
           ;; (github-pass (password-store-get-field "code/github" "token"))
           (archive-response
            (request
              (cond ((string= operation "executeSql") (concat dbadmin-host "/database/executeSql"))
                    ((string= operation "explainSql") (concat dbadmin-host "/database/explainSql"))
                    ((string= operation "queryTable") (concat dbadmin-host "/database/queryTable"))
                    ((string= operation "showTableStruct") (concat dbadmin-host "/database/showTableStruct"))
                    (t (concat dbadmin-host "/database/explainSql")))
              :type "POST" :parser 'json-read
              :headers `(("cookie" . ,dbadmin-cookie)) ;; ("Content-Type" . "application/json") ("charset" . "UTF-8")
              :data (cond
                     ((or (string= operation "executeSql") (string= operation "explainSql"))
                      `(
                        ("sql" . ,(buffer-substring-no-properties (region-beginning) (region-end)))
                        ("id" . ,(assoc-default "dbId" dbadmin-database))
                        ("dataBaseName" . ,(assoc-default "dbName" dbadmin-database))
                        ("page" . ,dbadmin-page-no)
                        ("limit" . ,dbadmin-page-size)
                        ("sqlNoCache" . "true")))
                     ((string= operation "queryTable")
                      `(
                        ("databaseId" . ,(assoc-default "dbId" dbadmin-database))
                        ("databaseName" . ,(assoc-default "dbName" dbadmin-database))))
                     ((string= operation "showTableStruct")
                      `(
                        ("databaseId" . ,(assoc-default "dbId" dbadmin-database))
                        ("databaseName" . ,(assoc-default "dbName" dbadmin-database))
                        ("tableName" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))
                     (t ()))
              :sync t))
           (rsp (request-response-data archive-response))
           (status (request-response-status-code archive-response)))
      (if (eq status 200)
          (cond
           ((or (string= operation "executeSql") (string= operation "explainSql"))
            (let* ((rows (assoc-default 'data rsp)))
              (if (eq (assoc-default 'code rsp) 0)
                  (if (eq rows [])
                      (progn
                        (dbadmin-check-buffer-exist)
                        (with-current-buffer (get-buffer-create dbadmin-buffer-name)
                          (let ((inhibit-read-only t))
                            (erase-buffer)
                            (insert (format "查询结果为空, pageNo:%s, pageSize:%s, 查询时间:%s, 条数:%s\n"
                                            dbadmin-page-no dbadmin-page-size (assoc-default 'executeTime rsp) (assoc-default 'count rsp))))))
                    (let* (
                           (title (mapcar (lambda (item) (decode-hex-string (string-replace "pre" "" (symbol-name (car item))))) (aref rows 0)))
                           (rows-decrypted ())
                           )
                      (dotimes (i (length rows))
                        (let* ((decrypted-item ()))
                          (dolist (item (aref rows i))
                            (push (string-replace "</xmp>" "" (string-replace "<xmp>" "" (cdr item))) decrypted-item)
                            )
                          (push (nreverse decrypted-item) rows-decrypted)
                          )
                        )
                      ;; (print title) ;; (print rows) ;; (print rows-decrypted)
                      (ctbl:create-table-component-buffer
                       :buffer (get-buffer-create dbadmin-buffer-name)
                       :model (ctbl:make-model-from-list
                               (nreverse rows-decrypted)
                               title
                               ))
                      (dbadmin-check-buffer-exist)
                      (with-current-buffer (get-buffer-create dbadmin-buffer-name)
                        (let ((inhibit-read-only t))
                          (goto-char (point-min))
                          (insert (format "pageNo:%s, pageSize:%s, 查询时间:%s, 条数:%s\n"
                                          dbadmin-page-no dbadmin-page-size (assoc-default 'executeTime rsp) (assoc-default 'count rsp)))
                          ))
                      )
                    )
                (message (assoc-default 'msg rsp)))
              ))
           ((string= operation "queryTable")
            (dbadmin-check-buffer-exist)
            (with-current-buffer (get-buffer-create dbadmin-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (goto-char (point-min))
                (insert (format "查询时间:%s, 条数:%s\n" (assoc-default 'executeTime rsp) (assoc-default 'count rsp)))
                (dotimes (i (length (assoc-default 'data rsp)))
                  (insert (format "%s\n"(aref (assoc-default 'data rsp) i)))
                  )
                )))
           ((string= operation "showTableStruct")
            (dbadmin-check-buffer-exist)
            (with-current-buffer (get-buffer-create dbadmin-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (goto-char (point-min))
                (insert (format "查询时间:%s, 条数:%s\n" (assoc-default 'executeTime rsp) (assoc-default 'count rsp)))
                (insert (format "%s" (assoc-default 'data rsp)))
                )))
           (t ()))
        ;; status ;; 405
        (message "cookie已过期")
        ))))

(global-set-key (kbd "C-c e") (lambda () (interactive) (dbadmin-exec "executeSql")))
(global-set-key (kbd "C-c C-e") (lambda () (interactive) (dbadmin-exec "executeSql")))
(global-set-key (kbd "C-c E") (lambda () (interactive) (dbadmin-exec "explainSql")))
(global-set-key (kbd "C-c t") (lambda () (interactive) (dbadmin-exec "queryTable")))
(global-set-key (kbd "C-c s") (lambda () (interactive) (dbadmin-exec "showTableStruct")))
(global-set-key (kbd "C-c M-c") (lambda () (interactive) (dbadmin-set-cookie)))
(global-set-key (kbd "C-c M-d") (lambda () (interactive) (dbadmin-choose-database)))

;; format table macro-function; F3; F4; name-last-kbd-macro; insert-kbd-macro;
(fset 'my/format-table
   (kmacro-lambda-form [?\M-< ?\C-s ?+ ?- return ?\C-a ?\C-  ?\M-< ?\C-w ?\C-k ?\C-k ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?| ?[ ? ?] ?* return ?| return ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?| return return ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?| ?$ return return ?\M-< ?\M-x ?q ?u ?e ?r ?y ?- ?r ?e ?p ?l ?a ?c ?e return ?| return tab return ?! ?\M-<] 0 "%d"))

;; ;; ---------------------------------------------------------------------------- dbadmin end

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
