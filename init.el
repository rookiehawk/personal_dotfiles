;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
   ;;                      ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
                         ("melpa" . "https://elpa.emacs-china.org/melpa/")))

(require 'package)
(package-initialize)

(setq package-selected-packages
      '(selectrum
	selectrum-prescient
	company
	company-prescient
	prescient
	ivy
	ivy-prescient
	swiper
	counsel
	smex
	monokai-theme
	smartparens
	js2-mode
	web-mode
	popwin
	airline-themes
	evil
	evil-surround
	magit
	git-gutter
	lsp-mode
	lsp-ui
	lsp-ivy
	company-lsp
	lsp-python-ms
	flycheck	
	dap-mode
	w3m
	yasnippet
))

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

(require 'selectrum)
(require 'prescient)
(require 'selectrum-prescient)
(require 'ivy-prescient)
(require 'company-prescient)

(selectrum-mode 1)
(selectrum-prescient-mode 1)
(ivy-prescient-mode 1)
(company-prescient-mode 1)
(with-eval-after-load "selectrum"
  (define-key selectrum-minibuffer-map (kbd "{") #'selectrum-previous-candidate)
  (define-key selectrum-minibuffer-map (kbd "}") #'selectrum-next-candidate))



(require 'company)
(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'conf-mode-hook #'company-mode)
(with-eval-after-load "company"
  (define-key company-active-map (kbd "{") #'company-select-previous)
  (define-key company-active-map (kbd "}") #'company-select-next))

(require 'smartparens-config)
(require 'smartparens)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'conf-mode-hook #'smartparens-mode)
(with-eval-after-load "smartparens"
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-r") #'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-s") #'sp-splice-sexp))

(autoload #'magit "magit" nil t)
(global-set-key (kbd "C-x g") #'magit)


(load-theme 'monokai t)

(require 'popwin)
(popwin-mode 1)

(require 'airline-themes)
(load-theme 'airline-light t)

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'conf-mode-hook #'display-line-numbers-mode)

(setq-default inhibit-startup-screen t
              indent-tabs-mode nil
              line-spacing 3)

(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "<down>") 'scroll-up-line)
(global-set-key (kbd "<up>") 'scroll-down-line)


;; dap-mode
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))




;; w3m
(require 'w3m-load)

;; evil-surround
(require 'evil-surround)



;; smex
(require 'smex) 

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; git-gutter
(global-git-gutter-mode +1)























;; ================================= normal 配置 ========================

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)


;; 显示行号
(global-linum-mode 1)

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

;; 禁用备份文件
(setq make-backup-files nil)
