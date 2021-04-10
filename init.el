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
	highlight-indentation
	projectile
	treemacs
	treemacs-evil
	treemacs-projectile
	treemacs-magit
	treemacs-icons-dired
	treemacs-persp
	lsp-treemacs
	winum
	use-package
	go-mode
	org-bullets
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
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
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


;; git-gutter
(global-git-gutter-mode +1)



(require 'winum)
(winum-mode 1) ;;(winum-mode)
;;c-x w <n>   c-x w `

;; lsp-python-ms
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook #'lsp) ; or lsp-deferred

;; go mode configuration
(require 'go-mode)
(require 'lsp-mode)
(setq lsp-gopls-use-placeholders nil)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)



;; highlight-indentation
(require 'highlight-indentation)
(setq highlight-indentation-mode t)
(setq highlight-indentation-current-column-mode t)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; swiper ivy counsel 这三个关系密切，主要关注的是minibuffer
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-height 10)
(setq ivy-initial-inputs-alist nil)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)



;; projectile
;; 试试这个命令  c-c p f
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; lsp-treemacs
(lsp-treemacs-sync-mode 1)


;; treemacs 相关默认设置
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil
          treemacs-load-theme			 "Default"
    	  treemacs-resize-icons 22)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


;; flycheck 语法检查
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  )


;; start lsp mode and yasnippet mode
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

























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
;; (set-frame-font "FiraCode Nerd Font Mono 12")
(set-frame-font "Monaco 12")
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
