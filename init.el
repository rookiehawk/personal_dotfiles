;;; Package --- init.el
;;; Commentary:
;;; Code:

;;; ================ normal configuration ==============
;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 显示行号
(global-linum-mode 1)

;; Set up the visible bell
(setq visible-bell t)

;; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 120)

;; 关闭启动帮助画面
(setq inhibit-splash-screen t)

;; 关闭缩进
;; (electric-indent-mode -1)

;; 内置的智能补全括号
(electric-pair-mode t)

;; 高亮括号
(show-paren-mode 1)

;; 更改显示字体大小 xx pt
;; (set-frame-font "FiraCode Nerd Font Mono 12")
;; (set-frame-font "Monaco 12")
;; (set-frame-font "YaHei Consolas Hybrid 12")
(setq linum-format "%d ") ;; 注意%d后有空格，即用空格将行号和代码隔开

;;; Set window size
(setq default-frame-alist '((width . 99) (height . 36)))

;; 禁用备份文件
;; (setq make-backup-files nil)

;; 关闭锁文件
(setq create-lockfiles nil)


;; load-theme
;; (load-theme 'tango-dark t)


;; utf-8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))



;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))



;;; ========================= package configuration ============================

;; Initialize package sources
(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
                         ("melpa" . "https://elpa.emacs-china.org/melpa/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)



(use-package diminish)


;; ivy swiper counsel
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 ("C-c g" . counsel-ag)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
	 ("C-x C-r" . counsel-recentf)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))


(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))



;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))



;;modeline
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))


;; super-save
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))


;; automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))


;; magit
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package magit-todos
  :defer t)


(use-package git-gutter
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2))


(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))




(use-package company
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t
	      company-idle-delay 0.1
	      company-echo-delay 0
              company-minimum-prefix-length 2
	      company-require-match nil
;;	      company-dabbrev-ignore-case nil
;;	      company-dabbrev-downcase nil
	      company-show-numbers t)
  :config
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous)))


;; 人工智能补全代码
(use-package company-tabnine
  :disabled
  :after 'company-mode
  'company-tabnine-mode
  :config
  (add-to-list 'company-backends #'company-tabnine))


;; 美化company
(use-package company-box
  :hook (company-mode . company-box-mode))

;; 代码片段
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets")))


;; web-mode
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2)
  :hook (html-mode . web-mode))



;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml\\'")


;; go-mode
(use-package go-mode
  :mode "\\.go\\'")


;; js2-mode
(use-package js2-mode
  :mode "\\.js\\'")

;; scss-mode
(use-package scss-mode
  :mode "\\.scss\\'")

;; emmet-mode
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (html-mode . emmet-mode)))

;; typescript-mode
(use-package typescript-mode
  :mode "\\.ts\\'")

;; vue support
(use-package vue-mode
  :mode "\\.vue\\'")


;; python-mode
(use-package python-mode
  :mode "\\.py\\'")




;; flycheck
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))


;; rainbow-mode
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))


;; winum
(use-package winum
  :config
  (winum-mode t))


;; evil
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


;; org-mode
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))


;; dired
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode t))




;; lsp
;; Emacs对语言服务器支持的插件
(use-package lsp-mode
  :defer 2
  :init (setq lsp-keymap-prefix "s-g")
  :commands (lsp)
  :hook (((js2-mode web-mode scss-mode python-mode go-mode vue-mode) . lsp)
         (lsp-mode . (lambda ()
                       (lsp-enable-which-key-integration))))
  :custom
  (lsp-idle-delay 200)
  (lsp-auto-guess-root nil)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1240))
  (lsp-eldoc-hook nil)
  (lsp-auto-configure t)
  (lsp-log-io nil)
  (lsp-prefer-flymake nil)
  (lsp-diagnostics-provide :flycheck)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting nil)
  (lsp-response-timeout 3)
  :bind (:map lsp-mode-map
			  ("C-c C-f" . lsp-format-buffer)
			  ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :config
  (define-key lsp-command-map (kbd "s-g") lsp-command-map)

    ;; 设置lsp的一些界面功能
  ;; 具体变量是干嘛的可以参考这里： https://emacs-lsp.github.io/lsp-mjjode/tutorials/how-to-turn-off/
  (setq lsp-lens-enable t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions t
        lsp-modeline-code-actions-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-prefer-capf t
        lsp-signature-render-documentation nil)

  (add-hook 'lsp-ui-imenu-mode-hook (lambda ()
                                      (display-line-numbers-mode -1))))

;; 微软的python语言服务器-新版
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3"))
  :custom
  (lsp-pyright-use-library-code-for-types t))  ; or lsp-deferred

;; 美化lsp-mode
(use-package lsp-ui
  :ensure t
  :commands (lsp-ui)
  ;; :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   ;; sideline
   lsp-ui-sideline-update-mode 'line
   ;; sideline
   lsp-ui-sideline-delay 1
   ;; lsp-ui-imenu列表自动刷新
   lsp-ui-imenu-auto-refresh t
   ;; lsp-ui-imenu列表刷新延迟
   lsp-ui-imenu-auto-refresh-delay 5.0)
  ;; peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; doc
  (setq lsp-ui-doc-enable t
        ;; 文档显示的位置
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil
        lsp-signature-render-documentation nil
        ;; 显示文档的延迟
        lsp-ui-doc-delay 2))











(provide 'init.el)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
