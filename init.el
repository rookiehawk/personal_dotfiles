;; ======================= 插件配置 ==========================

(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
;;     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;        ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像
;; 我这里使用了清华镜像源
     (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))



 ;; cl - Common Lisp Extension
 (require 'cl-lib)
 ;; Add Packages
 (defvar my/packages '(
  ;; --- Auto-completion ---
  company
  ;; --- Better Editor ---
  hungry-delete
  swiper
  counsel
  smartparens
  ;; --- Themes ---
  monokai-theme
  ;; solarized-theme
  popwin
  ) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (cl-loop for pkg in my/packages
    when (not (package-installed-p pkg)) do (return nil)
    finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
  (package-install pkg))))

 ;; Find Executable Path on OS X
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

;; ================================ 插件配置 =============================

;; hungry-delete 配置
(require 'hungry-delete)
(global-hungry-delete-mode)

;; swiper 配置
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
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


;; smartparens配置
(require 'smartparens)

;; popwin配置
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)





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
;; (setq inhibit-splash-screen 1)

;; 关闭缩进 (第二天中被去除)
;; (electric-indent-mode -1)

;; 更改显示字体大小 16pt
;; (set-frame-font "Monaco 11")
(set-frame-font "YaHei Consolas Hybrid 13")
(setq linum-format "%d ") ;; 注意%d后有空格，即用空格将行号和代码隔开

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)


;; 开启全局 Company 补全
(global-company-mode 1)

;; 设置光标
(setq-default cursor-type 'bar)

;; 关闭自动备份
(setq make-backup-files nil)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)


;; 当你选中一段文字 之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode 1)

;; 高亮当前行
(global-hl-line-mode 1)

;; 主题
(add-to-list 'my/packages 'monokai-theme)
;; 初始化加载主题
(load-theme 'monokai 1)


;; ==================== 根据分辨率大小，更改emacs的大小 ==========================
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)
;; ==================== 根据分辨率大小，更改emacs的大小 ==========================

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
