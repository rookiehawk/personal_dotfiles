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
;; (global-display-line-numbers-mode)

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
;; (setq linum-format "%d ") ;; 注意%d后有空格，即用空格将行号和代码隔开

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




;; ======================= package configuration =======================

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






;;; ======================= basic =================================
;; Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Highlight current line number
(use-package hlinum
  :config
  (hlinum-activate))

;; Better numbering
(use-package linum
  :ensure nil
  :config
  (setq linum-format "%3d ")
  :hook (prog-mode . display-line-numbers-mode))

;; Expand available key options
(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir)))
	      ;; Don't check remote files' presences
              recentf-keep '(file-remote-p file-readable-p))
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/recentf"))
  (setq recentf-auto-cleanup 'never) ; Don't conflict with tramp!
  (push (expand-file-name recentf-save-file) recentf-exclude))




;; hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))


;; smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))


(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))


;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after (projectile-mode)
  :config (counsel-projectile-mode))

(use-package neotree
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'nerd2)
  :bind
  ("<f8>" . neotree-current-dir-toggle)
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

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
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
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
          treemacs-workspace-switch-cleanup      nil)

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





;;; ================== ivy counsel swiper ========================

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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


(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
        ;; ("C-M-l" . counsel-imenu)
	 ("C-M-i" . counsel-imenu)
	 ("C-c k" . counsel-ag)
	 ("C-c l" . counsel-locate)
	 ("C-c g" . counsel-git)
	 ("C-x C-r" . counsel-recentf)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^



(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))


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
                       (if-let ((buffer (get-buffer cand)))))))))
                           ;; Don't mess with EXWM buffers
                           ;; (with-current-buffer buffer
                            ;;  (not (derived-mode-p 'exwm-mode)))))))))



;;; =================== completion ========================
;; 著名的Emacs补全框架
(use-package company 
  :defer 2 
  :init
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.1
	company-echo-delay 0
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil
	company-show-numbers t) 
  :bind (:map company-active-map
              ("M-n" . nil) 
              ("M-p" . nil) 
              ("C-n" . #'company-select-next) 
              ("C-p" . #'company-select-previous))
  :config
  (global-company-mode t)) 

;; 人工智能补全代码
(use-package company-tabnine
  :disabled 
  :ensure t 
  :after company 
  :config (add-to-list 'company-backends #'company-tabnine))

;; 各个语言的Debug工具
;; (use-package dap-mode
;;   :ensure t
;;   :functions dap-hydra/nil
;;   :diminish
;;   :bind (:map lsp-mode-map
;; 			  ("<f5>" . dap-debug)
;; 			  ("M-<f5>" . dap-hydra))
;;   :hook ((after-init . dap-mode)
;; 		 (dap-mode . dap-ui-mode)
;; 		 (python-mode . (lambda () (require 'dap-python)))
;; 		 ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))))


;; 美化company
(use-package company-box 
  :hook (company-mode . company-box-mode))

;; 代码片段
(use-package yasnippet 
  :defer 2
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets")))


;; company-prescient
(use-package company-prescient
  :after prescient
  :config
  (company-prescient-mode 1))

(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))

(add-to-list 'company-transformers 'company//sort-by-tabnine t)
;; `:separate`  使得不同 backend 分开排序
(add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))

;;; ======================== UI ===============================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-snazzy t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))





;;; ============================ evil =====================================
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))


(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; ==================================== window ================================

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))


(use-package winum
  :init (setq winum-keymap
	      (let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-²") 'winum-select-window-by-number)
		(define-key map (kbd "C-`") 'winum-select-window-0-or-10)
		(define-key map (kbd "M-1") 'winum-select-window-1)
		(define-key map (kbd "M-2") 'winum-select-window-2)
		(define-key map (kbd "M-3") 'winum-select-window-3)
		(define-key map (kbd "M-4") 'winum-select-window-4)
		(define-key map (kbd "M-5") 'winum-select-window-5)
		(define-key map (kbd "M-6") 'winum-select-window-6)
		(define-key map (kbd "M-7") 'winum-select-window-7)
		(define-key map (kbd "M-8") 'winum-select-window-8)
		(define-key map (kbd "M-9") 'winum-select-window-9)
		map))
  :config
  (winum-mode))


;;; ============================= org ================================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


;;; ========================= git ================================
;; Great git extensions

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package git-gutter
    :custom
    (git-gutter:modified-sign "~")		; 
    (git-gutter:added-sign    "+")		; 
    (git-gutter:deleted-sign  "-")		; 
    :custom-face
    (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1))


;;; ================================ code ==============================

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((python-mode go-mode js-mode js2-mode vue-mode web-mode yaml-mode typescript-mode scss-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
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
	      ("C-c C-r" . lsp-ui-peek-find-references)
	      ("C-c C-j" . lsp-ui-peek-find-definitions)
	      ("C-c i"   . lsp-ui-peek-find-implementation)
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

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
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
	lsp-ui-doc-max-width 35
	lsp-ui-doc-max-height 8
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-use-webkit t
        lsp-ui-sideline-enable nil
        lsp-signature-render-documentation nil
        ;; 显示文档的延迟
        lsp-ui-doc-delay 2))
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred



;; vue-mode
(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode))

;; js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;; js2-refactor
(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :hook (html-mode . web-mode))

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)
         (html-mode . emmet-mode)))

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (setq scss-compile-at-save t))


(use-package typescript-mode
  :mode "\\.ts\\'")


(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest)
  (use-package go-tag
    :config (setq go-tag-args (list "-transform" "camelcase"))))




;; json-mode
(use-package json-mode)

;; 














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
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c")))))
