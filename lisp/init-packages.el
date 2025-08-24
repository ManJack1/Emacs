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

(use-package evil-surround
  :ensure t
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

(use-package doom-themes
  :ensure t)

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
  ;; 初始不启用，后面通过 hook 控制
  ;; (centaur-tabs-mode t)  ; 注释掉这行
  
  ;; 自定义 buffer 分组
  (defun my/centaur-tabs-buffer-groups ()
    "Custom buffer grouping for centaur-tabs"
    (list
     (cond
      ;; 隐藏系统 buffer (以 * 开头的)
      ((string-match-p "^\\*" (buffer-name)) "System")
      ;; 隐藏以空格开头的临时 buffer
      ((string-match-p "^ " (buffer-name)) "Hidden")
      ;; 文件 buffer
      ((buffer-file-name) "Files")
      ;; Dired 模式
      ((memq major-mode '(dired-mode)) "Dired")
      ;; 其他
      (t "Common"))))
  
  (setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)
  
  ;; 只显示文件 buffer 的标签
  (defun my/centaur-tabs-hide-tab (buffer)
    "Hide certain tabs from being displayed"
    (let ((name (buffer-name buffer)))
      (or
       ;; 隐藏以 * 开头的系统 buffer
       (string-match-p "^\\*" name)
       ;; 隐藏以空格开头的临时 buffer
       (string-match-p "^ " name)
       ;; 隐藏 magit 相关 buffer
       (string-match-p "magit" name)
       ;; 可以添加更多需要隐藏的 buffer
       )))
  
  (setq centaur-tabs-hide-tab-function #'my/centaur-tabs-hide-tab)
  
  ;; Cycle 范围
  (setq centaur-tabs-cycle-scope 'tabs)
  
  ;; 标签字体颜色设置
  (set-face-attribute 'centaur-tabs-selected nil
                      :foreground "#DDA0DD"      ; 亮紫色
                      :background 'unspecified  ; 使用主题背景
                      :weight 'bold)
  
  (set-face-attribute 'centaur-tabs-unselected nil
                      :foreground "#888888"      ; 修改为灰色，更容易区分
                      :background 'unspecified
                      :weight 'normal)
  
  (set-face-attribute 'centaur-tabs-default nil
                      :foreground "#000000"
                      :background 'unspecified)
  
  ;; 其他有用的设置
  (setq centaur-tabs-set-icons t)           ; 显示图标
  (setq centaur-tabs-gray-out-icons 'buffer) ; 灰化非活动标签图标
  (setq centaur-tabs-set-close-button nil)   ; 不显示关闭按钮
  (setq centaur-tabs-set-modified-marker t)  ; 显示修改标记
  (setq centaur-tabs-modified-marker "●"))   ; 修改标记样式

;; 智能启用 centaur-tabs 的函数
(defun my/update-centaur-tabs-mode ()
  "智能启用 centaur-tabs：只在有多个文件 buffer 时启用"
  (let ((file-buffers (cl-count-if 
                       (lambda (buf)
                         (and (buffer-file-name buf)
                              ;; 确保是可见的 buffer
                              (not (string-match-p "^\\*" (buffer-name buf)))
                              (not (string-match-p "^ " (buffer-name buf)))))
                       (buffer-list))))
    (if (> file-buffers 1)
        (unless centaur-tabs-mode
          (centaur-tabs-mode 1))
      (when centaur-tabs-mode
        (centaur-tabs-mode -1)))))

;; 添加 hook，但避免频繁调用
(defvar my/centaur-tabs-timer nil
  "Timer for delayed centaur-tabs update")

(defun my/delayed-update-centaur-tabs ()
  "延迟更新 centaur-tabs 状态，避免频繁调用"
  (when my/centaur-tabs-timer
    (cancel-timer my/centaur-tabs-timer))
  (setq my/centaur-tabs-timer
        (run-with-timer 0.1 nil #'my/update-centaur-tabs-mode)))

;; 使用延迟更新避免性能问题
(add-hook 'buffer-list-update-hook #'my/delayed-update-centaur-tabs)

;; 在特定事件后也检查一下
(add-hook 'find-file-hook #'my/delayed-update-centaur-tabs)
(add-hook 'kill-buffer-hook #'my/delayed-update-centaur-tabs)


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

(use-package consult-yasnippet
  :ensure t)

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

;;; SIS (Smart Input Source) cross-platform configuration
;;; Supports macOS and Linux

(use-package sis
  :ensure t
  :config
  ;; Cross-platform input source configuration
  (cond
   ;; macOS configuration
   ((eq system-type 'darwin)
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.ITABC"
     ))
   
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

;; undo-fu 配置（专为 evil 优化）
(use-package undo-fu
  :ensure t
  :config
  ;; Evil 兼容设置
  (setq undo-fu-allow-undo-in-region t)
  ;; 设置 undo 限制
  (setq undo-limit 67108864)           ; 64MB
  (setq undo-strong-limit 100663296)   ; 96MB  
  (setq undo-outer-limit 1006632960))  ; 960MB

;; 持久化 undo 历史
(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (undo-fu-session-global-mode)
  ;; 设置保存目录
  (setq undo-fu-session-directory 
        (expand-file-name "undo-fu-session/" user-emacs-directory))
  ;; 忽略某些文件
  (setq undo-fu-session-ignore-glob-patterns
        '("/tmp/*" "*.tmp" "*.log" "**/COMMIT_EDITMSG" "**/.git/*"))
  ;; 启用压缩
  (setq undo-fu-session-compression 'gz)
  ;; 线性 undo（更符合 Vim 习惯）
  (setq undo-fu-session-linear t))

;; Evil 配置，使用 undo-fu
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)  ; 关键设置
  :config
  (evil-mode 1)
  ;; 绑定 undo/redo 键
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))

(use-package org-appear
  :ensure t
  :hook (org-mode org-appear-mode))

(provide 'init-packages)
;;; init-packages.el ends here
