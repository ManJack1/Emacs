;;; init-packages.el --- Packages configuration with straight.el

;; ===============================================
;; straight.el 初始化
;; ===============================================

;; 禁用 package.el
(setq package-enable-at-startup nil)

;; 安装 straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 安装 use-package
(straight-use-package 'use-package)

;; 配置 use-package 默认使用 straight.el
(setq straight-use-package-by-default t)

;; 性能优化
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; ===============================================
;; 包配置
;; ===============================================

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  
  ;; 智能的单词包围函数
  (defun my/surround-add-word ()
    "Add surround to current word, handles edge cases better"
    (interactive)
    (let ((char (read-char "Surround word with: ")))
      (save-excursion
        ;; 确保在单词上
        (unless (looking-at "\\w")
          (re-search-forward "\\w" nil t)
          (backward-char))
        
        ;; 移动到单词边界
        (let ((start (progn (backward-word) (point)))
              (end (progn (forward-word) (point))))
          (evil-surround-region start end 'exclusive char)))))
  
  ;; 绑定快捷键
  (define-key evil-normal-state-map "gsa" 'my/surround-add-word)
  (define-key evil-visual-state-map "gsa" 'evil-surround-region)
  (define-key evil-normal-state-map "gsd" 'evil-surround-delete)
  (define-key evil-normal-state-map "gsr" 'evil-surround-change))

(use-package doom-themes)

;; 图标支持
(use-package nerd-icons)

;; 为补全添加图标
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Marginalia - 丰富的注释
(use-package marginalia
  :init
  (marginalia-mode))

;; Vertico - 垂直补全界面
(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  :init
  (vertico-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

;; 删除重复的 marginalia 配置
;; (use-package marginalia
;;   :config
;;   (marginalia-mode))

(use-package embark)

(use-package consult)

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (setq treesit-auto-install t))

(use-package centaur-tabs
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
                      :foreground "#000000"  ;; 白色
                      :background 'unspecified  ;; 使用 unspecified 而不是 nil
                      :weight 'normal)
  
  (set-face-attribute 'centaur-tabs-default nil
                      :foreground "#000000"
                      :background 'unspecified))  ;; 使用 unspecified 而不是 nil

(defun my/update-centaur-tabs-mode ()
  "Enable centaur-tabs only if more than 1 buffer."
  (if (> (length (cl-remove-if-not #'buffer-file-name (buffer-list))) 1)
      (centaur-tabs-mode 1)
    (centaur-tabs-mode -1)))

(add-hook 'buffer-list-update-hook #'my/update-centaur-tabs-mode)

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-background-alpha 0.3)
  (setq avy-style 'at-full)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package company
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
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; 安装官方 snippet 集
(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet)

(with-eval-after-load 'yasnippet
  ;; 让yasnippet更宽松地匹配
  (setq yas-key-syntaxes '("w_" "w_." "w_.()" "^ "))
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-triggers-in-field t))

(with-eval-after-load 'company
  (defun my/company-backend-with-yas (backends)
    "将yasnippet添加到现有后端中"
    (if (and (listp backends) (memq 'company-capf backends))
        (append backends '(:with company-yasnippet))
      backends))
  
  ;; 智能组合后端
  (setq company-backends
        '((company-capf :with company-yasnippet)  ; LSP + snippet
          company-dabbrev-code
          company-keywords  
          company-files
          company-dabbrev))
  
  ;; 设置company参数
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t))

(use-package recentf
  :straight nil  ; 使用 Emacs 内置版本
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))

(use-package simple
  :straight nil  ; 使用 Emacs 内置版本
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)))

;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :config
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode-line-mode t))

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后
(use-package doom-modeline
  :init
  (doom-modeline-mode t))

(use-package consult-todo
  :after (consult keycast))

(use-package savehist
  :straight nil  ; 使用 Emacs 内置版本
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package saveplace
  :straight nil  ; 使用 Emacs 内置版本
  :hook (after-init . save-place-mode))

;; 使用 straight.el 安装最新版本的 org
(use-package org
  :straight (:host github :repo "emacs-mirror/emacs" :files ("lisp/org/*.el")))

;; 安装 org-contrib
(use-package org-contrib
  :after org)

;;; SIS (Smart Input Source) cross-platform configuration
;;; Supports macOS and Linux

(use-package sis
  :config
  ;; Cross-platform input source configuration
  (cond
   ;; macOS configuration
   ((eq system-type 'darwin)
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.ITABC"))
   
   ;; Linux fcitx5 configuration
   ((eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5)))
  
  ;; Enable all modes
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t)
  
  ;; Evil integration - 修改这部分
  (defvar sis--saved-input-source nil)
  
  (defun sis--evil-insert-enter ()
    "默认进入英文输入法，除非是在中文环境下"
    ;; 总是先设置为英文
    (sis-set-english))
  
  (defun sis--evil-insert-exit ()
    "离开插入模式时保存当前输入法并切换到英文"
    (setq sis--saved-input-source (sis-get))
    (sis-set-english))
  
  ;; Hook into evil state changes
  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook #'sis--evil-insert-enter)
    (add-hook 'evil-insert-state-exit-hook #'sis--evil-insert-exit)
    ;; 确保 normal mode 也是英文
    (add-hook 'evil-normal-state-entry-hook #'sis-set-english)))

(provide 'init-packages)
;;; init-packages.el ends here
