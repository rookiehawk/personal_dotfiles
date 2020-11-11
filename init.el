;;; Tune the GC
;; The default settings are too conservative on modern machines making Emacs
;; spend too much time collecting garbage in alloc-heavy code.
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.3)

;; ======================= 插件配置 ==========================

(when (>= emacs-major-version 24)
       (require 'package)
       (package-initialize)
 ;; 我这里使用了清华镜像源
	 (setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
		 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
(setq my-package-list ;启动时自动安装的插件
      '(
        find-file-in-project
        web-mode php-mode js2-mode
        ivy ivy-rich counsel hydra swiper avy
        counsel-etags dumb-jump 
        magit
		company
		objed
	    ;;	popwin
        ;; hungry-delete
        ))
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))



;; ========================== 插件详细配置 ========================

;; ivy counsel swiper

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f2> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


;; company
(add-hook 'after-init-hook 'global-company-mode)
;; 开启全局 Company 补全
;; (global-company-mode 1)

;; theme
(load-theme 'monokai t)


;; objed emacs下面的text object插件
(objed-mode 1)

;; hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

























;; ================================= normal 配置 ========================

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
(setq cursor-type 'bar)

;; 关闭启动帮助画面
;; (setq inhibit-splash-screen t)

;; 关闭缩进
;; (electric-indent-mode -1)

;; 内置的智能补全括号
(electric-pair-mode t)

;; 更改显示字体大小 xx pt
(set-frame-font "FiraCode Nerd Font Mono 12")
;; (set-frame-font "YaHei Consolas Hybrid 12")
(setq linum-format "%d ") ;; 注意%d后有空格，即用空格将行号和代码隔开

;;; Set window size
(setq default-frame-alist '((width . 98) (height . 35)))

;; 快速打开配置文件
(defun open-init-file()
    (interactive)
      (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

;; 设置光标
(setq-default cursor-type 'bar)

;; 关闭自动备份
(setq make-backup-files nil)

;;; Disable lock files
(setq create-lockfiles nil)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 当你选中一段文字 之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode t)

;; 高亮当前行
(global-hl-line-mode 1)


;;; Use fucking UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 3)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)

;;; Ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; History saving
(require 'savehist)
(setq history-length 1024)
(setq history-delete-duplicates t)
(setq search-ring-max 1024)
(setq regexp-search-ring-max 1024)
(setq savehist-additional-variables '(extended-command-history file-name-history search-ring regexp-search-ring))
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
(savehist-mode)

;; 禁用蜂鸣声
(setq visible-bell 0)

;; emacs中,tab键常常被空格键取代功能，下面的设置，使tab键永远都是tab键
;; 在各种mode下
(global-set-key (kbd "TAB") 'self-insert-command)
;; 仅仅在text-mode下
;; (define-key text-mode-map (kbd "TAB") 'self-insert-command)

;; tab 4个空格而不是8个
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(hungry-delete monokai-theme magit dumb-jump counsel-etags avy hydra counsel ivy-rich php-mode web-mode find-file-in-project)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
