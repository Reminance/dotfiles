;;----------------------------------------------------------------------------
;; Optional package stuff
;;----------------------------------------------------------------------------

;; ;; fzf
;; (use-package fzf
;;   :bind (("M-O" . fzf)))

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

;; ;; dap-java
;; (use-package dap-java :ensure nil)

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
;;           :vars '((user-mail-address . "xucheng2@work-account.com")
;;                   (user-full-name . "xucheng2")
;;                   (smtpmail-smtp-user  . "xucheng2@work-account.com")
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

;; ;; 写js可用的模式
;; (use-package js2-mode)

;;(use-package markdown-mode
;;  :mode "\\.md\\'"
;;  :mode "\\.text\\'"
;;  :mode "\\.markdown\\'")
;;(use-package markdown-mode+
;;  :defer t
;;  :hook (markdown-mode . (lambda () (require 'markdown-mode+)))
;;  :config
;;  ())


;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult start

;; ;; Enable vertico
;; (use-package vertico
;;   :init
;;   (vertico-mode)

;;   ;; Different scroll margin
;;   ;; (setq vertico-scroll-margin 0)

;;   ;; Show more candidates
;;   ;; (setq vertico-count 20)

;;   ;; Grow and shrink the Vertico minibuffer
;;   ;; (setq vertico-resize t)

;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   ;; (setq vertico-cycle t)
;;   )

;; ;; Optionally use the `orderless' completion style. See
;; ;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; ;; dispatcher. Additionally enable `partial-completion' for file path
;; ;; expansion. `partial-completion' is important for wildcard support.
;; ;; Multiple files can be opened at once with `find-file' if you enter a
;; ;; wildcard. You may also give the `initials' completion style a try.
;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; Alternatively try `consult-completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;; ;; Enable richer annotations using the Marginalia package
;; (use-package marginalia
;;   ;; Either bind `marginalia-cycle` globally or only in the minibuffer
;;   :bind (("M-A" . marginalia-cycle)
;;          :map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))

;;   ;; The :init configuration is always executed (Not lazy!)
;;   :init

;;   ;; Must be in the :init section of use-package such that the mode gets
;;   ;; enabled right away. Note that this forces loading the package.
;;   (marginalia-mode))

;; (use-package embark
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
;; (setq prefix-help-command 'embark-prefix-help-command)

;; ;; Example configuration for Consult
;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings (mode-specific-map)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c k" . consult-kmacro)
;;          ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;          ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g I" . consult-imenu-multi)
;;          ;; M-s bindings (search-map)
;;          ("M-s d" . consult-find)
;;          ("M-s D" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s L" . consult-line-multi)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
;;          ;; Isearch integration
;;          ("M-s e" . consult-isearch-history)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;          ;; Minibuffer history
;;          :map minibuffer-local-map
;;          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;          ("M-r" . consult-history))                ;; orig. previous-matching-history-element

;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init
;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Optionally replace `completing-read-multiple' with an enhanced version.
;;   (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config
;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key (kbd "M-."))
;;   ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-bookmark consult--source-recent-file
;;    consult--source-project-recent-file
;;    :preview-key (kbd "M-."))

;;   (setq consult-ripgrep-args "rg --null --column --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden -g \"!.git\" .")

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; By default `consult-project-function' uses `project-root' from project.el.
;;   ;; Optionally configure a different project root function.
;;   ;; There are multiple reasonable alternatives to chose from.
;;   ;;;; 1. project.el (the default)
;;   ;; (setq consult-project-function #'consult--default-project--function)
;;   ;;;; 2. projectile.el (projectile-project-root)
;;   ;; (autoload 'projectile-project-root "projectile")
;;   ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;   ;;;; 3. vc.el (vc-root-dir)
;;   ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;   ;;;; 4. locate-dominating-file
;;   ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

;;   (setq consult-async-min-input 2) ;; Minimum number of letters needed, before asynchronous process is called.
;; )

;; ;; ---------------------------------------------------------------------------- vertico orderless marginalia embark consult end


