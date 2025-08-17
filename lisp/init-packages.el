
;;; init-packages.el --- Packages configuration


;; 设置包源
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; 如果没有安装 use-package，先安装它
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; 图标支持
(use-package nerd-icons
  :ensure t)

;; 为补全添加图标
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Marginalia - 丰富的注释
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Vertico - 垂直补全界面
(use-package vertico
  :ensure t
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t)

(use-package consult
  :ensure t)

(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (setq treesit-auto-install t))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  
  ;; 自定义 buffer 分组
  (defun my/centaur-tabs-buffer-groups ()
    "Custom buffer grouping for centaur-tabs"
    (list
     (cond
      ((string-match-p "^\*" (buffer-name)) nil)
      ((string-match-p "^ " (buffer-name)) nil)
      ((buffer-file-name) "Files")
      ((memq major-mode '(dired-mode)) "Dired")
      (t "Common"))))
  
  (setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)
  
  ;; Cycle 范围
  (setq centaur-tabs-cycle-scope 'tabs)
  
  ;; 标签字体颜色设置（修复 background 警告）
  (set-face-attribute 'centaur-tabs-selected nil
                      :foreground "#DDA0DD"  ;; 亮紫色
                      :background 'unspecified  ;; 使用 unspecified 而不是 nil
                      :weight 'bold)
  
  (set-face-attribute 'centaur-tabs-unselected nil
                      :foreground "#FFFFFF"  ;; 白色
                      :background 'unspecified  ;; 使用 unspecified 而不是 nil
                      :weight 'normal)
  
  (set-face-attribute 'centaur-tabs-default nil
                      :foreground "#FFFFFF"
                      :background 'unspecified))  ;; 使用 unspecified 而不是 nil

(defun my/update-centaur-tabs-mode ()
  "Enable centaur-tabs only if more than 1 buffer."
  (if (> (length (cl-remove-if-not #'buffer-file-name (buffer-list))) 1)
      (centaur-tabs-mode 1)
    (centaur-tabs-mode -1)))

(add-hook 'buffer-list-update-hook #'my/update-centaur-tabs-mode)

(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  (setq avy-background-alpha 0.3)
  (setq avy-style 'at-full)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers nil)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))
  
  ;; 禁用 Tab 和 Shift-Tab 键，只允许 C-n 和 C-p
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "S-TAB") nil)
  (define-key company-active-map (kbd "<backtab>") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; 安装 yasnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; 安装官方 snippet 集
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)





;; 让 company 用 lsp-mode 的补全
(use-package company-capf
  :ensure nil
  :after (company lsp-mode)
  :init
  (setq company-backends
      '((company-capf :with company-yasnippet) ; 先 LSP + snippet
        company-files                           ; 然后文件名
        company-dabbrev)))                       ; 最后缓冲区文字)

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))

(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :ensure t
  :config
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode-line-mode t))


;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t))

(use-package consult-todo
  :ensure t
  :after (consult keycast))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300)
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; 安装org 之前，一定要配置 use-package 不使用内置的org 版本，可以使用本段代码最后面的代码，具体位置可以参考视频
(use-package org
  :pin melpa
  :ensure t)

;; 添加 NonGNU ELPA 源
(add-to-list 'package-archives 
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

;; 安装 org-contrib
(use-package org-contrib
  :ensure t
  :after org)
(provide 'init-packages)
;;; init-packages.el ends here
