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
  (setq mac-command-modifier 'meta) ;; use command key as meta/alt
  ;; (setq mac-option-modifier 'none) ;; If ‘none’, the key is ignored by Emacs and retains its standard meaning.
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
                           ;; :name "JetbrainsMono Nerd Font"
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
;; disable `show-trailing-whitespace' in unpropriate place
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
(setq scroll-conservatively 101)
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))
(setq hscroll-step 1)
(setq scroll-margin 3)
(global-set-key [wheel-right] (lambda() (interactive) (scroll-left 5)))
(global-set-key [wheel-left] (lambda() (interactive) (scroll-right 5)))
;; ;; t means point keeps its screen position if the scroll command moved it to next screen
;; (setq scroll-preserve-screen-position t)

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
                             ;; (or (getenv "CPPFLAGS") "-O0")
                             ;; (or (getenv "CFLAGS") "-Wall -Werror -Wextra -pedantic -g")  ;; (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             "-O0"
                             "-Wall -Werror -Wextra -pedantic -g"
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
  (set-scroll-bar-mode nil))(when (fboundp 'menu-bar-mode)
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

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

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
;; sql-mode from sql.el
;;----------------------------------------------------------------------------

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'."
  (end-of-buffer) ;; could be optional ?
  (concat "\n" output))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first
            (lambda ()
              (toggle-truncate-lines t))
            ))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; org-mode
;; ----------------------------------------------------------------------------
(defvar org-notes-dir "~/workspace/work-tools/org-notes"
  "My org notes directory.")
(defvar org-notes-file "notes.org"
  "My org notes file.")
(defvar org-personal-file "personal.org"
  "My org personal file.")
(define-key leader-key "fi" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key leader-key "fc" (lambda () (interactive) (find-file (expand-file-name "custom.el" user-emacs-directory))))
(define-key leader-key "fm" (lambda () (interactive) (find-file (expand-file-name "machine-specific.el" user-emacs-directory))))
(define-key leader-key "fn" (lambda () (interactive) (find-file (format "%s/%s" org-notes-dir org-notes-file))))
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
(setq org-startup-with-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images nil)
(setq org-default-notes-file (format "%s/%s" org-notes-dir org-notes-file))
(setq org-agenda-files (list
                        (format "%s/%s" org-notes-dir org-notes-file)
                        (format "%s/%s" org-notes-dir org-personal-file)
                        ))
(add-hook 'org-mode-hook 'org-indent-mode)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Type C-h v org-agenda-window-setup for other options.
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-restore-windows-after-quit t)

(setq org-directory org-notes-dir)
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline (format "%s/%s" org-notes-dir org-notes-file) "Tasks")
         ;; Prompt for tag
         "* TODO %?\t%^g\n Entered on %U\n  %i\n  %a")
        ("o" "Someday" entry (file+headline (format "%s/%s" org-notes-dir org-personal-file) "Tasks")
         "* SOMEDAY %?")
        ("s" "Code Snippet" entry (file+datetree (format "%s/%s" org-notes-dir "snippet.org"))
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
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
                         ("org" . "http://mirrors.tuna.tsinghuna.edu.cn/elpa/org/")
                         ))
;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                          ("melpa-stable" . "http://elpa.emacs-china.org/stable-melpa/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ))

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

;; ======================================== gtags start ========================================
;; ;; (setq load-path (cons "/opt/homebrew/bin/global" load-path))
;; ;; (setq load-path (cons "/opt/homebrew/bin/gtags" load-path))
;; (setq load-path (cons "/opt/homebrew/Cellar/global/6.6.8/share/gtags/gtags.el" load-path))
;; ;; (autoload 'gtags-mode "gtags" "" t)
;; (require 'gtags)
;; (use-package ggtags)

;; ;; (defun gtags-root-dir ()
;; ;;   "Return GTAGS root directory or nil if doesn't exist."
;; ;;   ;; (interactive)
;; ;;   (with-temp-buffer
;; ;;     (if (zerop (call-process "global" nil t nil "-pr"))
;; ;;         (buffer-substring (point-min) (1- (point-max)))
;; ;;       nil)))
;; ;; (defun gtags-update ()
;; ;;   "Make GTAGS incremental update."
;; ;;   (call-process "global" nil nil nil "-u"))
;; ;; (defun gtags-update-hook ()
;; ;;   "Add hook while in gtags-mode."
;; ;;   (when (gtags-root-dir)
;; ;;     (gtags-update)))
;; ;; (add-hook 'after-save-hook #'gtags-update-hook)

;; ;; (setq gtags-mode-hook
;; ;;       '(lambda ()
;; ;;          (local-set-key "\M-t" 'gtags-find-tag)
;; ;;          (local-set-key "\M-r" 'gtags-find-rtag)
;; ;;          (local-set-key "\M-s" 'gtags-find-symbol)
;; ;;          (local-set-key "\C-t" 'gtags-pop-stack)
;; ;;          ))

;; (setq java-mode-hook
;;       '(lambda ()
;;          (gtags-mode 1)))
;; ======================================== gtags end ========================================

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
  ;; :bind ("M-@" . er/expand-region))
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region)
  )

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

;; evil
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

;; (use-package evil-collection
;;   ;; :disabled
;;   :after evil
;;   :config
;;   (evil-collection-init))

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
  :demand t
  :diminish company-mode
  :bind (("C-<tab>" . company-complete))
  :bind (
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ;; ("<tab>" . company-complete-selection)
         ;; ("SPC" . company-abort)
         )
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0
          company-echo-delay 0
          company-tooltip-align-annotations t
          company-tooltip-limit 10
          company-minimum-prefix-length 1
          company-show-numbers t
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-global-modes '(not magit-status-mode)
          )))

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

;; python ~/workspace/work-tools/python/fetch_dbadmin_dict.py
;; script to load dbadmin table fields to dict/sql-mode file
(use-package company-dict
  :config
  ;; Where to look for dictionary files. Default is ~/.emacs.d/dict
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  ;; Optional: if you want it available everywhere
  (add-to-list 'company-backends 'company-dict)
  )

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

(progn
  (use-package all-the-icons)
  (use-package all-the-icons-dired
    :hook ('dired-mode . 'all-the-icons-dired-mode))
  (use-package emojify
    :custom (emojify-emojis-dir (expand-file-name "var/emojis" user-emacs-directory))))

;; ;;;;; ctable  ;; custom zenburn themes
;;    `(ctbl:face-cell-select ((t (:background nil :foreground ,zenburn-green))))
;;    `(ctbl:face-continue-bar ((t (:background ,zenburn-bg-05 :foreground ,zenburn-bg))))
;;    `(ctbl:face-row-select ((t (:background nil :foreground ,zenburn-green))))
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

;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-monokai-pro t)
;;   ;; Enable flashing mode-line on errors
;;   ;; (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :config (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-c y" "有道翻译")
  :bind (("C-c y y" . 'youdao-dictionary-search-at-point+)
         ("C-c y g" . 'youdao-dictionary-search-at-point-posframe)
         ("C-c y p" . 'youdao-dictionary-play-voice-at-point)
         ("C-c y r" . 'youdao-dictionary-search-and-replace)
         ("C-c y i" . 'youdao-dictionary-search-from-input)))

;; golang
(use-package go-mode)
(use-package yaml-mode :defer t)
(use-package json-mode
  :mode "\\.json\\'")


;; ---------------------------------------------------------------------------- lsp-mode start
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

;; ---------------------------------------------------------------------------- lsp-mode end

;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper start
;; other similar packages to prescient: vertico consult selectrum prescient for completion

(use-package swiper
  :ensure t
  :bind ("C-S-s" . 'swiper)
  )

(use-package counsel
  :bind (
         ("M-x" . 'counsel-M-x)
         ("C-x b" . 'counsel-ibuffer)
         ("C-M-j" . 'counsel-switch-buffer) ;; skip corrent buffer, more efficient
         ("C-x d" . 'counsel-dired)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x C-r" . 'counsel-recentf)
         ("C-c C-r" . 'ivy-resume)
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c f" . 'counsel-fzf)
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
  (setq counsel-rg-base-command "rg --hidden --ignore-case --max-columns 300 --no-heading --line-number --color never %s -g \"!.git\" -g !doc -g !themes -g !quelpa -g !backup ")
  (setq counsel-fzf-cmd "fd --hidden --exclude={.git,site-lisp,etc/snippets,themes,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,backup} --type f | fzf -f \"%s\" --algo=v1")
  ;; Integration with 'projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

(use-package ivy
  :diminish ivy-mode
  :config
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-fuzzy)))
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

;; ;; ---------------------------------------------------------------------------- Ivy, Counsel and Swiper end


;; ;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult start

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

;; (use-package marginalia
;;   ;; Either bind `marginalia-cycle' globally or only in the minibuffer
;;   :bind (("M-A" . marginalia-cycle)
;;          :map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))
;;   :init
;;   (marginalia-mode))

;; (use-package embark
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
;; (setq prefix-help-command 'embark-prefix-help-command)

;; ;; Example configuration for Consult
;; (use-package consult
;;   ;; ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings (mode-specific-map)
;;          ;;        ("C-c h" . consult-history)
;;          ;;        ("C-c m" . consult-mode-command)
;;          ;;        ("C-c k" . consult-kmacro)
;;          ;;        ;; C-x bindings (ctl-x-map)
;;          ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ;;        ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
;;          ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;          ;;        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;          ;;        ;; Custom M-# bindings for fast register access
;;          ;;        ("M-#" . consult-register-load)
;;          ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ;;        ("C-M-#" . consult-register)
;;          ;;        ;; Other custom bindings
;;          ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ;;        ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;;        ;; M-g bindings (goto-map)
;;          ;;        ("M-g e" . consult-compile-error)
;;          ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ;;        ("M-g m" . consult-mark)
;;          ;;        ("M-g k" . consult-global-mark)
;;          ;;        ("M-g i" . consult-imenu)
;;          ;;        ("M-g I" . consult-imenu-multi)
;;          ;;        ;; M-s bindings (search-map)
;;          ;;        ("M-s d" . consult-find)
;;          ;;        ("M-s D" . consult-locate)
;;          ;;        ("M-s g" . consult-grep)
;;          ;;        ("M-s G" . consult-git-grep)
;;                 ("M-s r" . consult-ripgrep)
;;                 ("M-S-f" . consult-ripgrep)
;;          ;;        ("M-s l" . consult-line)
;;          ;;        ("M-s L" . consult-line-multi)
;;          ;;        ("M-s m" . consult-multi-occur)
;;          ;;        ("M-s k" . consult-keep-lines)
;;          ;;        ("M-s u" . consult-focus-lines)
;;          ;;        ;; Isearch integration
;;          ;;        ("M-s e" . consult-isearch-history)
;;          ;;        :map isearch-mode-map
;;          ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ;;        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;          ;;        ;; Minibuffer history
;;          ;;        :map minibuffer-local-map
;;          ;;        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;          ;;        ("M-r" . consult-history)                ;; orig. previous-matching-history-element
;;          )

;;   :hook (completion-list-mode . consult-preview-at-point-mode)
;;   :init
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)
;;   (advice-add #'register-preview :override #'consult-register-window)
;;   (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)
;;   :config
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-bookmark consult--source-recent-file
;;    consult--source-project-recent-file
;;    :preview-key (kbd "M-."))
;;   (setq consult-ripgrep-args "rg --null --color=never --max-columns=300 --ignore-case --no-heading --line-number --hidden -g \"!.git\" -g !themes -g !quelpa -g !backup .")
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")
;;   (setq consult-async-min-input 2) ;; Minimum number of letters needed, before asynchronous process is called.
;;   )

;; ;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult end

(use-package ctable)

(use-package request)

;; ;; ---------------------------------------------------------------------------- dbadmin start

;; OzsgZGVmaW5lIGl0IGluIG1hY2hpbmUtc3BlY2lmaWMuZWwKOzsgKGRlZnZhciBkYmFkbWluLWhvc3QgImh0dHBzOi8vZGJhZG1pbi1jbi1uZXcuZGV2LnNoZWluY29ycC5jbiIpCjs7IChkZWZ2YXIgZGJhZG1pbi1jb29raWUgIiIpCihkZWZ2YXIgZGJhZG1pbi1kYXRhYmFzZSAnKCgiZGJJZCIgLiAiMTk5IikgKCJkYk5hbWUiIC4gIndtc193YXJlaG91c2Vfd2l0aGluIikpKQooZGVmdmFyIGRiYWRtaW4tcGFnZS1ubyAxKQooZGVmdmFyIGRiYWRtaW4tcGFnZS1zaXplIDEwMDApCihkZWZ2YXIgZGJhZG1pbi1idWZmZXItbmFtZSAiKmRiYWRtaW4qIikKKGRlZnZhciBkYmFkbWluLWRhdGFiYXNlLWFsaXN0ICcoCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICg/MSAid3dzIiAobGFtYmRhICgpIChtZXNzYWdlICJDaG9vc2VuOiB3d3MiKSAoc2V0cSBkYmFkbWluLWRhdGFiYXNlICcoKCJkYklkIiAuICIxOTkiKSAoImRiTmFtZSIgLiAid21zX3dhcmVob3VzZV93aXRoaW4iKSkpKSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKD8yICJ3c3MiIChsYW1iZGEgKCkgKG1lc3NhZ2UgIkNob29zZW46IHdzcyIpIChzZXRxIGRiYWRtaW4tZGF0YWJhc2UgJygoImRiSWQiIC4gIjEwNCIpICgiZGJOYW1lIiAuICJuZXdzaXRldGVzdCIpKSkpKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoPzMgIndtcyIgKGxhbWJkYSAoKSAobWVzc2FnZSAiQ2hvb3Nlbjogd21zIikgKHNldHEgZGJhZG1pbi1kYXRhYmFzZSAnKCgiZGJJZCIgLiAiMjUiKSAoImRiTmFtZSIgLiAibmV3c2l0ZXRlc3QiKSkpKSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKD80ICJzdGF0IiAobGFtYmRhICgpIChtZXNzYWdlICJDaG9vc2VuOiBzdGF0IikgKHNldHEgZGJhZG1pbi1kYXRhYmFzZSAnKCgiZGJJZCIgLiAiMTQ3IikgKCJkYk5hbWUiIC4gIndtc19zdGF0IikpKSkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICg/NSAiYXJjaGl2ZV8xIiAobGFtYmRhICgpIChtZXNzYWdlICJDaG9vc2VuOiBhcmNoaXZlXzEiKSAoc2V0cSBkYmFkbWluLWRhdGFiYXNlICcoKCJkYklkIiAuICI5MCIpICgiZGJOYW1lIiAuICJ3bXNfYXJjaGl2ZXIiKSkpKSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKSkKCihkZWZ1biBkYmFkbWluLWNob29zZS1kYXRhYmFzZSAoKQogICJEYmFkbWluIGNob29zZSBkYXRhYmFzZS4iCiAgKGludGVyYWN0aXZlKQogIChsZXQgKChjaG9pY2UgKHJlYWQtY2hhci1jaG9pY2UgKG1hcGNvbmNhdCAobGFtYmRhIChpdGVtKSAoZm9ybWF0ICIlYzogJXMiIChjYXIgaXRlbSkgKGNhZHIgaXRlbSkpKSBkYmFkbWluLWRhdGFiYXNlLWFsaXN0ICI7ICIpCiAgICAgICAgICAgICAgICAgIChtYXBjYXIgIydjYXIgZGJhZG1pbi1kYXRhYmFzZS1hbGlzdCkpKSkKICAgIChmdW5jYWxsIChudGggMiAoYXNzb2MgY2hvaWNlIGRiYWRtaW4tZGF0YWJhc2UtYWxpc3QpKSkpKQoKKGRlZnVuIGRiYWRtaW4tc2V0LXBhZ2Utc2l6ZSAoKQogICJQbGVhc2UgZW50ZXIgZGJhZG1pbiBwYWdlIHNpemUuIgogIChpbnRlcmFjdGl2ZSkKICAobGV0KiAoCiAgICAgICAgIChwYWdlLXNpemUgKHJlYWQtbnVtYmVyICJQbGVhc2UgZW50ZXIgZGJhZG1pbiBwYWdlIHNpemU6IikpCiAgICAgICAgICkKICAgIChzZXRxIGRiYWRtaW4tcGFnZS1zaXplIHBhZ2Utc2l6ZSkKICAgIChtZXNzYWdlICJTdWNjZXNzZnVsbHkgc2V0IGRiYWRtaW4tcGFnZS1zaXplOiAlcyIgcGFnZS1zaXplKSkKICApCgooZGVmdW4gZGJhZG1pbi1zZXQtY29va2llICgpCiAgIlBsZWFzZSBlbnRlciBkYmFkbWluIGNvb2tpZS4iCiAgKGludGVyYWN0aXZlKQogIChsZXQqICgKICAgICAgICAgKGNvb2tpZS1pbnB1dCAocmVhZC1zdHJpbmcgIkRiYWRtaW4gY29va2llOiIpKQogICAgICAgICApCiAgICAoc2V0cSBkYmFkbWluLWNvb2tpZSBjb29raWUtaW5wdXQpCiAgICAoc2F2ZS1leGN1cnNpb24KICAgICAgKGxldCAoKGJ1ZiAoZmluZC1maWxlLW5vc2VsZWN0IChleHBhbmQtZmlsZS1uYW1lICJtYWNoaW5lLXNwZWNpZmljLmVsIiB1c2VyLWVtYWNzLWRpcmVjdG9yeSkpKSkKICAgICAgICAoc2V0LWJ1ZmZlciBidWYpCiAgICAgICAgKGJlZ2lubmluZy1vZi1idWZmZXIpCiAgICAgICAgKHdoaWxlIChzZWFyY2gtZm9yd2FyZC1yZWdleHAgIihkZWZ2YXIgZGJhZG1pbi1jb29raWUgXCIuKlwiKSIgbmlsIHQpCiAgICAgICAgICAocmVwbGFjZS1tYXRjaCAoZm9ybWF0ICIoZGVmdmFyIGRiYWRtaW4tY29va2llIFwiJXNcIikiIGNvb2tpZS1pbnB1dCkgdCBuaWwpKQogICAgICAgIChzYXZlLWJ1ZmZlcikKICAgICAgICAoa2lsbC1idWZmZXIpKSkKICAgIChtZXNzYWdlICJTdWNjZXNzZnVsbHkgc2V0IGRiYWRtaW4tY29va2llOiAlcyIgY29va2llLWlucHV0KSkKICApCgooZGVmdW4gZGVjb2RlLWhleC1zdHJpbmcgKGhleC1zdHJpbmcpCiAgIkhvdyBkbyBJIGNvbnZlcnQgYSBIRVgtU1RSSU5HIGludG8gQVNDSUkgdXNpbmcgZWxpc3A/ICBodHRwczovL3N0YWNrb3ZlcmZsb3cuY29tL3F1ZXN0aW9ucy8xMjAwMzIzMS9ob3ctZG8taS1jb252ZXJ0LWEtc3RyaW5nLW9mLWhleC1pbnRvLWFzY2lpLXVzaW5nLWVsaXNwLiIKICAobGV0ICgocmVzIG5pbCkpCiAgICAoZG90aW1lcyAoaSAoLyAobGVuZ3RoIGhleC1zdHJpbmcpIDIpIChhcHBseSAjJ2NvbmNhdCAocmV2ZXJzZSByZXMpKSkKICAgICAgKGxldCAoKGhleC1ieXRlIChzdWJzdHJpbmcgaGV4LXN0cmluZyAoKiAyIGkpICgqIDIgKCsgaSAxKSkpKSkKICAgICAgICAocHVzaCAoZm9ybWF0ICIlYyIgKHN0cmluZy10by1udW1iZXIgaGV4LWJ5dGUgMTYpKSByZXMpKSkpKQoKKGRlZnVuIGRiYWRtaW4tY2hlY2stYnVmZmVyLWV4aXN0KCkKICAiRGJhZG1pbiBjaGVjayBidWZmZXIgZXhpc3QsIGdldCBvciBjcmVhdGUgaXQuIgogIChpbnRlcmFjdGl2ZSkKICAoY29uZCAoKGVxIChnZXQtYnVmZmVyIGRiYWRtaW4tYnVmZmVyLW5hbWUpICh3aW5kb3ctYnVmZmVyIChzZWxlY3RlZC13aW5kb3cpKSkgOzsgKG1lc3NhZ2UgIlZpc2libGUgYW5kIGZvY3VzZWQiKQogICAgICAgICApCiAgICAgICAgKChnZXQtYnVmZmVyLXdpbmRvdyAoZ2V0LWJ1ZmZlciBkYmFkbWluLWJ1ZmZlci1uYW1lKSkgOzsgKG1lc3NhZ2UgIlZpc2libGUgYW5kIHVuZm9jdXNlZCIpCiAgICAgICAgICkKICAgICAgICAodAogICAgICAgICAoc3BsaXQtd2luZG93LWJlbG93KQogICAgICAgICAod2luZG1vdmUtZG93bikKICAgICAgICAgKHN3aXRjaC10by1idWZmZXIgZGJhZG1pbi1idWZmZXItbmFtZSkgOzsgKG1lc3NhZ2UgIk5vdCB2aXNpYmxlIikKICAgICAgICAgKSkKICApCgooZGVmdW4gZGJhZG1pbi1leGVjIChvcGVyYXRpb24pCiAgIkV4ZWMgT1BFUkFUSU9OIHZpYSBkYmFkbWluIGh0dHAgcmVxdWVzdC4iCiAgKGludGVyYWN0aXZlKQogIChpZiAoYW5kIChzdHJpbmc9IChidWZmZXItc3Vic3RyaW5nLW5vLXByb3BlcnRpZXMgKHJlZ2lvbi1iZWdpbm5pbmcpIChyZWdpb24tZW5kKSkgIiIpCiAgICAgICAgICAgKG9yIChzdHJpbmc9IG9wZXJhdGlvbiAiZXhlY3V0ZVNxbCIpIChzdHJpbmc9IG9wZXJhdGlvbiAiZXhwbGFpblNxbCIpIChzdHJpbmc9IG9wZXJhdGlvbiAic2hvd1RhYmxlU3RydWN0IikpKQogICAgICAodXNlci1lcnJvciAiRXJyb3IsIGJlY2F1c2U6ICVzIiAic3FsIGNhbm5vdCBiZSBudWxsIikgbmlsKQogIChzYXZlLWV4Y3Vyc2lvbgogICAgKHJlcXVlc3QKICAgICAgKGNvbmQgKChzdHJpbmc9IG9wZXJhdGlvbiAiZXhlY3V0ZVNxbCIpIChjb25jYXQgZGJhZG1pbi1ob3N0ICIvZGF0YWJhc2UvZXhlY3V0ZVNxbCIpKQogICAgICAgICAgICAoKHN0cmluZz0gb3BlcmF0aW9uICJleHBsYWluU3FsIikgKGNvbmNhdCBkYmFkbWluLWhvc3QgIi9kYXRhYmFzZS9leHBsYWluU3FsIikpCiAgICAgICAgICAgICgoc3RyaW5nPSBvcGVyYXRpb24gInF1ZXJ5VGFibGUiKSAoY29uY2F0IGRiYWRtaW4taG9zdCAiL2RhdGFiYXNlL3F1ZXJ5VGFibGUiKSkKICAgICAgICAgICAgKChzdHJpbmc9IG9wZXJhdGlvbiAic2hvd1RhYmxlU3RydWN0IikgKGNvbmNhdCBkYmFkbWluLWhvc3QgIi9kYXRhYmFzZS9zaG93VGFibGVTdHJ1Y3QiKSkKICAgICAgICAgICAgKHQgKGNvbmNhdCBkYmFkbWluLWhvc3QgIi9kYXRhYmFzZS9leHBsYWluU3FsIikpKQogICAgICA6dHlwZSAiUE9TVCIgOnBhcnNlciAnanNvbi1yZWFkCiAgICAgIDpoZWFkZXJzIGAoKCJjb29raWUiIC4gLGRiYWRtaW4tY29va2llKSkgOzsgKCJDb250ZW50LVR5cGUiIC4gImFwcGxpY2F0aW9uL2pzb24iKSAoImNoYXJzZXQiIC4gIlVURi04IikKICAgICAgOmRhdGEgKGNvbmQKICAgICAgICAgICAgICgob3IgKHN0cmluZz0gb3BlcmF0aW9uICJleGVjdXRlU3FsIikgKHN0cmluZz0gb3BlcmF0aW9uICJleHBsYWluU3FsIikpCiAgICAgICAgICAgICAgYCgKICAgICAgICAgICAgICAgICgic3FsIiAuICwoYnVmZmVyLXN1YnN0cmluZy1uby1wcm9wZXJ0aWVzIChyZWdpb24tYmVnaW5uaW5nKSAocmVnaW9uLWVuZCkpKQogICAgICAgICAgICAgICAgKCJpZCIgLiAsKGFzc29jLWRlZmF1bHQgImRiSWQiIGRiYWRtaW4tZGF0YWJhc2UpKQogICAgICAgICAgICAgICAgKCJkYXRhQmFzZU5hbWUiIC4gLChhc3NvYy1kZWZhdWx0ICJkYk5hbWUiIGRiYWRtaW4tZGF0YWJhc2UpKQogICAgICAgICAgICAgICAgKCJwYWdlIiAuICxkYmFkbWluLXBhZ2Utbm8pCiAgICAgICAgICAgICAgICAoImxpbWl0IiAuICxkYmFkbWluLXBhZ2Utc2l6ZSkKICAgICAgICAgICAgICAgICgic3FsTm9DYWNoZSIgLiAidHJ1ZSIpKSkKICAgICAgICAgICAgICgoc3RyaW5nPSBvcGVyYXRpb24gInF1ZXJ5VGFibGUiKQogICAgICAgICAgICAgIGAoCiAgICAgICAgICAgICAgICAoImRhdGFiYXNlSWQiIC4gLChhc3NvYy1kZWZhdWx0ICJkYklkIiBkYmFkbWluLWRhdGFiYXNlKSkKICAgICAgICAgICAgICAgICgiZGF0YWJhc2VOYW1lIiAuICwoYXNzb2MtZGVmYXVsdCAiZGJOYW1lIiBkYmFkbWluLWRhdGFiYXNlKSkpKQogICAgICAgICAgICAgKChzdHJpbmc9IG9wZXJhdGlvbiAic2hvd1RhYmxlU3RydWN0IikKICAgICAgICAgICAgICBgKAogICAgICAgICAgICAgICAgKCJkYXRhYmFzZUlkIiAuICwoYXNzb2MtZGVmYXVsdCAiZGJJZCIgZGJhZG1pbi1kYXRhYmFzZSkpCiAgICAgICAgICAgICAgICAoImRhdGFiYXNlTmFtZSIgLiAsKGFzc29jLWRlZmF1bHQgImRiTmFtZSIgZGJhZG1pbi1kYXRhYmFzZSkpCiAgICAgICAgICAgICAgICAoInRhYmxlTmFtZSIgLiAsKGJ1ZmZlci1zdWJzdHJpbmctbm8tcHJvcGVydGllcyAocmVnaW9uLWJlZ2lubmluZykgKHJlZ2lvbi1lbmQpKSkpKQogICAgICAgICAgICAgKHQgKCkpKQogICAgICA7OyA6c3luYyB0ICA7OyDlvILmraXor7fmsYLlkozmuLLmn5Mg5LiN6Zi75aGe5pON5L2cCiAgICAgIDpjb21wbGV0ZQogICAgICAoY2wtZnVuY3Rpb24KICAgICAgIChsYW1iZGEgKCZrZXkgcmVzcG9uc2UgJmFsbG93LW90aGVyLWtleXMpCiAgICAgICAgIChsZXQqICgocnNwIChyZXF1ZXN0LXJlc3BvbnNlLWRhdGEgcmVzcG9uc2UpKSkKICAgICAgICAgICAoaWYgKGVxIChyZXF1ZXN0LXJlc3BvbnNlLXN0YXR1cy1jb2RlIHJlc3BvbnNlKSAyMDApCiAgICAgICAgICAgICAgIChjb25kCiAgICAgICAgICAgICAgICAoKG9yIChzdHJpbmc9IG9wZXJhdGlvbiAiZXhlY3V0ZVNxbCIpIChzdHJpbmc9IG9wZXJhdGlvbiAiZXhwbGFpblNxbCIpKQogICAgICAgICAgICAgICAgIChsZXQqICgocm93cyAoYXNzb2MtZGVmYXVsdCAnZGF0YSByc3ApKSkKICAgICAgICAgICAgICAgICAgIChpZiAoZXEgKGFzc29jLWRlZmF1bHQgJ2NvZGUgcnNwKSAwKQogICAgICAgICAgICAgICAgICAgICAgIChpZiAoZXEgcm93cyBbXSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgKHByb2duCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKGRiYWRtaW4tY2hlY2stYnVmZmVyLWV4aXN0KQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICh3aXRoLWN1cnJlbnQtYnVmZmVyIChnZXQtYnVmZmVyLWNyZWF0ZSBkYmFkbWluLWJ1ZmZlci1uYW1lKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKGxldCAoKGluaGliaXQtcmVhZC1vbmx5IHQpKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoZXJhc2UtYnVmZmVyKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoaW5zZXJ0IChmb3JtYXQgIuaXoOaVsOaNriwgcGFnZTolcywgc2l6ZTolcywgY29zdDolcywgY291bnQ6JXMsIOafpeivouaXtumXtDpbJXNdXG4iCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBkYmFkbWluLXBhZ2Utbm8gZGJhZG1pbi1wYWdlLXNpemUgKGFzc29jLWRlZmF1bHQgJ2V4ZWN1dGVUaW1lIHJzcCkgKGFzc29jLWRlZmF1bHQgJ2NvdW50IHJzcCkgKGZvcm1hdC10aW1lLXN0cmluZyAiJVktJW0tJWQgJUg6JU06JVMuJTNOIikpKSkpKQogICAgICAgICAgICAgICAgICAgICAgICAgKGxldCogKAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICh0aXRsZSAobWFwY2FyIChsYW1iZGEgKGl0ZW0pIChkZWNvZGUtaGV4LXN0cmluZyAoc3RyaW5nLXJlcGxhY2UgInByZSIgIiIgKHN5bWJvbC1uYW1lIChjYXIgaXRlbSkpKSkpIChhcmVmIHJvd3MgMCkpKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIChyb3dzLWRlY3J5cHRlZCAoKSkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgIChkb3RpbWVzIChpIChsZW5ndGggcm93cykpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKGxldCogKChkZWNyeXB0ZWQtaXRlbSAoKSkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoZG9saXN0IChpdGVtIChhcmVmIHJvd3MgaSkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIChwdXNoIChzdHJpbmctcmVwbGFjZSAiPC94bXA+IiAiIiAoc3RyaW5nLXJlcGxhY2UgIjx4bXA+IiAiIiAoY2RyIGl0ZW0pKSkgZGVjcnlwdGVkLWl0ZW0pKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKHB1c2ggKG5yZXZlcnNlIGRlY3J5cHRlZC1pdGVtKSByb3dzLWRlY3J5cHRlZCkKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgIDs7IChwcmludCB0aXRsZSkgOzsgKHByaW50IHJvd3MpIDs7IChwcmludCByb3dzLWRlY3J5cHRlZCkKICAgICAgICAgICAgICAgICAgICAgICAgICAgKGN0Ymw6Y3JlYXRlLXRhYmxlLWNvbXBvbmVudC1idWZmZXIKICAgICAgICAgICAgICAgICAgICAgICAgICAgIDpidWZmZXIgKGdldC1idWZmZXItY3JlYXRlIGRiYWRtaW4tYnVmZmVyLW5hbWUpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICA6bW9kZWwgKGN0Ymw6bWFrZS1tb2RlbC1mcm9tLWxpc3QKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKG5yZXZlcnNlIHJvd3MtZGVjcnlwdGVkKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0aXRsZQogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApKQogICAgICAgICAgICAgICAgICAgICAgICAgICAoZGJhZG1pbi1jaGVjay1idWZmZXItZXhpc3QpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICh3aXRoLWN1cnJlbnQtYnVmZmVyIChnZXQtYnVmZmVyLWNyZWF0ZSBkYmFkbWluLWJ1ZmZlci1uYW1lKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgIChsZXQgKChpbmhpYml0LXJlYWQtb25seSB0KSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIChnb3RvLWNoYXIgKHBvaW50LW1pbikpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoaW5zZXJ0IChmb3JtYXQgInBhZ2U6JXMsIHNpemU6JXMsIGNvc3Q6JXMsIGNvdW50OiVzLCDmn6Xor6Lml7bpl7Q6WyVzXVxuIgogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRiYWRtaW4tcGFnZS1ubyBkYmFkbWluLXBhZ2Utc2l6ZSAoYXNzb2MtZGVmYXVsdCAnZXhlY3V0ZVRpbWUgcnNwKSAoYXNzb2MtZGVmYXVsdCAnY291bnQgcnNwKSAoZm9ybWF0LXRpbWUtc3RyaW5nICIlWS0lbS0lZCAlSDolTTolUy4lM04iKSkpCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApKQogICAgICAgICAgICAgICAgICAgICAgICAgICApKQogICAgICAgICAgICAgICAgICAgICAobWVzc2FnZSAoYXNzb2MtZGVmYXVsdCAnbXNnIHJzcCkpKQogICAgICAgICAgICAgICAgICAgKSkKICAgICAgICAgICAgICAgICgoc3RyaW5nPSBvcGVyYXRpb24gInF1ZXJ5VGFibGUiKQogICAgICAgICAgICAgICAgIChkYmFkbWluLWNoZWNrLWJ1ZmZlci1leGlzdCkKICAgICAgICAgICAgICAgICAod2l0aC1jdXJyZW50LWJ1ZmZlciAoZ2V0LWJ1ZmZlci1jcmVhdGUgZGJhZG1pbi1idWZmZXItbmFtZSkKICAgICAgICAgICAgICAgICAgIChsZXQgKChpbmhpYml0LXJlYWQtb25seSB0KSkKICAgICAgICAgICAgICAgICAgICAgKGVyYXNlLWJ1ZmZlcikKICAgICAgICAgICAgICAgICAgICAgKGdvdG8tY2hhciAocG9pbnQtbWluKSkKICAgICAgICAgICAgICAgICAgICAgKGluc2VydCAoZm9ybWF0ICJjb3N0OiVzLCBjb3VudDolcywg5p+l6K+i5pe26Ze0Olslc11cbiIgKGFzc29jLWRlZmF1bHQgJ2V4ZWN1dGVUaW1lIHJzcCkgKGFzc29jLWRlZmF1bHQgJ2NvdW50IHJzcCkgKGZvcm1hdC10aW1lLXN0cmluZyAiJVktJW0tJWQgJUg6JU06JVMuJTNOIikpKQogICAgICAgICAgICAgICAgICAgICAoZG90aW1lcyAoaSAobGVuZ3RoIChhc3NvYy1kZWZhdWx0ICdkYXRhIHJzcCkpKQogICAgICAgICAgICAgICAgICAgICAgIChpbnNlcnQgKGZvcm1hdCAiJXNcbiIoYXJlZiAoYXNzb2MtZGVmYXVsdCAnZGF0YSByc3ApIGkpKSkpCiAgICAgICAgICAgICAgICAgICAgICkpKQogICAgICAgICAgICAgICAgKChzdHJpbmc9IG9wZXJhdGlvbiAic2hvd1RhYmxlU3RydWN0IikKICAgICAgICAgICAgICAgICAoZGJhZG1pbi1jaGVjay1idWZmZXItZXhpc3QpCiAgICAgICAgICAgICAgICAgKHdpdGgtY3VycmVudC1idWZmZXIgKGdldC1idWZmZXItY3JlYXRlIGRiYWRtaW4tYnVmZmVyLW5hbWUpCiAgICAgICAgICAgICAgICAgICAobGV0ICgoaW5oaWJpdC1yZWFkLW9ubHkgdCkpCiAgICAgICAgICAgICAgICAgICAgIChlcmFzZS1idWZmZXIpCiAgICAgICAgICAgICAgICAgICAgIChnb3RvLWNoYXIgKHBvaW50LW1pbikpCiAgICAgICAgICAgICAgICAgICAgIChpbnNlcnQgKGZvcm1hdCAiY29zdDolcywgY291bnQ6JXMsIOafpeivouaXtumXtDpbJXNdXG4iIChhc3NvYy1kZWZhdWx0ICdleGVjdXRlVGltZSByc3ApIChhc3NvYy1kZWZhdWx0ICdjb3VudCByc3ApIChmb3JtYXQtdGltZS1zdHJpbmcgIiVZLSVtLSVkICVIOiVNOiVTLiUzTiIpKSkKICAgICAgICAgICAgICAgICAgICAgKGluc2VydCAoZm9ybWF0ICIlcyIgKGFzc29jLWRlZmF1bHQgJ2RhdGEgcnNwKSkpCiAgICAgICAgICAgICAgICAgICAgICkpKQogICAgICAgICAgICAgICAgKHQgKCkpKQogICAgICAgICAgICAgOzsgc3RhdHVzIDs7IDQwNQogICAgICAgICAgICAgKG1lc3NhZ2UgImNvb2tpZeW3sui/h+acnyIpKQogICAgICAgICAgICkKICAgICAgICAgKSkKICAgICAgKSkpCgooZ2xvYmFsLXNldC1rZXkgKGtiZCAiQy1jIGUiKSAobGFtYmRhICgpIChpbnRlcmFjdGl2ZSkgKGRiYWRtaW4tZXhlYyAiZXhlY3V0ZVNxbCIpKSkKKGdsb2JhbC1zZXQta2V5IChrYmQgIkMtYyBFIikgKGxhbWJkYSAoKSAoaW50ZXJhY3RpdmUpIChkYmFkbWluLWV4ZWMgImV4cGxhaW5TcWwiKSkpCihnbG9iYWwtc2V0LWtleSAoa2JkICJDLWMgdCIpIChsYW1iZGEgKCkgKGludGVyYWN0aXZlKSAoZGJhZG1pbi1leGVjICJxdWVyeVRhYmxlIikpKQooZ2xvYmFsLXNldC1rZXkgKGtiZCAiQy1jIHMiKSAobGFtYmRhICgpIChpbnRlcmFjdGl2ZSkgKGRiYWRtaW4tZXhlYyAic2hvd1RhYmxlU3RydWN0IikpKQooZ2xvYmFsLXNldC1rZXkgKGtiZCAiQy1jIE0tYyIpIChsYW1iZGEgKCkgKGludGVyYWN0aXZlKSAoZGJhZG1pbi1zZXQtY29va2llKSkpCihnbG9iYWwtc2V0LWtleSAoa2JkICJDLWMgTS1kIikgKGxhbWJkYSAoKSAoaW50ZXJhY3RpdmUpIChkYmFkbWluLWNob29zZS1kYXRhYmFzZSkpKQoKOzsgZm9ybWF0IHRhYmxlIG1hY3JvLWZ1bmN0aW9uOyBGMzsgRjQ7IG5hbWUtbGFzdC1rYmQtbWFjcm87IGluc2VydC1rYmQtbWFjcm87Cihmc2V0ICdteS9mb3JtYXQtdGFibGUKICAgICAgKGttYWNyby1sYW1iZGEtZm9ybSBbP1xNLTwgP1xDLXMgPysgPy0gcmV0dXJuID9cQy1hID9cQy0gID9cTS08ID9cQy13ID9cQy1rID9cQy1rID9cTS14ID9yID9lID9wID9sID9hID9jID9lID8tID9yID9lID9nID9lID94ID9wIHJldHVybiA/fCA/XFsgPyA/XF0gPyogcmV0dXJuID98IHJldHVybiA/XE0tPCA/XE0teCA/ciA/ZSA/cCA/bCA/YSA/YyA/ZSA/LSA/ciA/ZSA/ZyA/ZSA/eCA/cCByZXR1cm4gP14gP3wgcmV0dXJuIHJldHVybiA/XE0tPCA/XE0teCA/ciA/ZSA/cCA/bCA/YSA/YyA/ZSA/LSA/ciA/ZSA/ZyA/ZSA/eCA/cCByZXR1cm4gP3wgPyQgcmV0dXJuIHJldHVybiA/XE0tPCA/XE0teCA/cSA/dSA/ZSA/ciA/eSA/LSA/ciA/ZSA/cCA/bCA/YSA/YyA/ZSByZXR1cm4gP3wgcmV0dXJuIHRhYiByZXR1cm4gPyEgP1xNLTxdIDAgIiVkIikpCgo=

;; ;; ---------------------------------------------------------------------------- dbadmin end

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
