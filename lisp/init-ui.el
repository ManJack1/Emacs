
;;; init-ui.el --- UI configuration

;; 字体设置
;; 设置默认字体为 JetBrains Mono
(set-face-attribute 'default nil
                    :font "Maple Mono NF"
                    :height 160) ; 14pt，高度以1/10pt为单位

;; 设置中文字体为 LXGW WenKai Mono
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "LXGW WenKai Mono")))

;; 设置粗体
(set-face-attribute 'bold nil
                    :font "Maple Mono NF"
                    :weight 'bold)

;; 设置斜体
(set-face-attribute 'italic nil
                    :font "Maple Mono NF"
                    :slant 'italic)

;; 基础界面配置
(setq frame-title-format '("%b"))
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)

;; 设置相对行号
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; 设置可视铃声
(setq visible-bell t)

;; 其他界面配置
(delete-selection-mode 1)
(global-hl-line-mode 1)
(show-paren-mode t)

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . my/enable-indent-guides-with-colors)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┊
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0.1
        highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-suppress-auto-error t)
  
  ;; 设置颜色
  (defun my/set-neon-blue-indent-guides ()
    "Set neon blue colors for indent guides - only innermost level."
    (let ((is-dark-theme (string-match-p "dark\\|black" 
                                        (symbol-name (or (car custom-enabled-themes) 'default)))))
      (if is-dark-theme
          (progn
            (set-face-foreground 'highlight-indent-guides-character-face "#3a3a3a")
            (set-face-foreground 'highlight-indent-guides-top-character-face "#00ffff")
            (set-face-foreground 'highlight-indent-guides-stack-character-face "#3a3a3a"))
        (progn
          (set-face-foreground 'highlight-indent-guides-character-face "#c0c0c0")
          (set-face-foreground 'highlight-indent-guides-top-character-face "#0080ff")
          (set-face-foreground 'highlight-indent-guides-stack-character-face "#c0c0c0")))))
  
  ;; 启用模式并设置颜色的函数
  (defun my/enable-indent-guides-with-colors ()
    "Enable indent guides mode and set custom colors."
    (highlight-indent-guides-mode 1)
    (my/set-neon-blue-indent-guides))
  
  ;; 修复的连续缩进线函数
  (defun my/ensure-continuous-guides ()
    "Ensure indent guides are continuous even with empty lines."
    (when (and (bound-and-true-p highlight-indent-guides-mode)
               (fboundp 'highlight-indent-guides--clear-cache))  ; 检查函数是否存在
      (save-excursion
        (beginning-of-line)
        (when (looking-at-p "^[[:space:]]*$")
          (highlight-indent-guides--clear-cache)
          (font-lock-flush (line-beginning-position) (line-end-position))))))
  
  ;; 主题切换时更新所有 buffer 的颜色
  (defun my/update-all-indent-guides-colors ()
    "Update indent guides colors in all buffers."
    (my/set-neon-blue-indent-guides)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when highlight-indent-guides-modE
          (when (fboundp 'highlight-indent-guIdes--clear-cache)
            (highlight-indent-guides--clear-cache))
          (when (derived-mode-p 'prog-mode)
            (font-lock-flush))))))
  
  (advice-add 'load-theme :after 
              (lambda (&rest _) (my/update-all-indent-guides-colors))))

(pixel-scroll-precision-mode t) 


;; === 可选：Evil 模式支持 ===
(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  :config
  ;; Evil 模式下的额外快捷键
  (define-key evil-treemacs-state-map (kbd "o") #'treemacs-visit-node-in-most-recently-used-window)
  (define-key evil-treemacs-state-map (kbd "O") #'treemacs-visit-node-in-horizontal-split)
  (define-key evil-treemacs-state-map (kbd "v") #'treemacs-visit-node-in-vertical-split)
  (define-key evil-treemacs-state-map (kbd "t") #'treemacs-visit-node-in-new-tab)
  (define-key evil-treemacs-state-map (kbd "q") #'treemacs-quit))

;; === 可选：Projectile 集成 ===
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; === 可选：Magit 集成 ===
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;; === 可选：Perspective 集成 ===
(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

;; === 可选：Tab Bar 集成 ===
(use-package treemacs-tab-bar
  :ensure t
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;; === 全局快捷键设置（NeoTree 风格） ===

;; 方案 1: 使用 general.el（推荐）
(use-package general
  :ensure t
  :config
  ;; Leader key 设置（SPC）
  (general-evil-setup t)
  (general-create-definer my-leader-def
    :keymaps 'override
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  
  ;; 简化的快捷键 - 只保留核心功能
  (my-leader-def
    "e" 'treemacs                     ; SPC e - 直接使用 treemacs 命令
    "E" 'my-treemacs-find-file))      ; SPC E - 在 treemacs 中找到当前文件

;; 方案 2: 使用 which-key + 手动绑定
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  ;; 手动设置 SPC 作为 leader key
  (define-prefix-command 'my-leader-map)
  
  ;; 绑定 C-SPC 到 leader map（适用于所有模式）
  (global-set-key (kbd "C-SPC") 'my-leader-map)
  
  ;; 如果使用 evil，在 normal 和 visual 模式下绑定 SPC
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC") 'my-leader-map)
    (define-key evil-visual-state-map (kbd "SPC") 'my-leader-map))
  
  ;; 简化的快捷键 - 只保留核心功能
  (define-key my-leader-map (kbd "e") 'treemacs)
  (define-key my-leader-map (kbd "E") 'my-treemacs-find-file)
  
  ;; which-key 描述
  (which-key-add-key-based-replacements
    "C-SPC e" "treemacs"
    "C-SPC E" "find file in treemacs"
    "SPC e" "treemacs"
    "SPC E" "find file in treemacs"))

;; === 简化的键盘导航 ===
;; 只保留最实用的导航方式

;; 使用 winum 进行窗口切换（官方推荐）
(use-package winum
  :ensure t
  :config
  (winum-mode)
  ;; M-0 切换到 treemacs（官方推荐绑定）
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

;; === 简化的窗口管理 ===
;; 移除了复杂的 advice，保持简单的行为

;; === 自定义主题样式（可选）===
(with-eval-after-load 'treemacs
  ;; 自定义 treemacs 的外观，使其更像 NeoTree
  (custom-set-faces
   '(treemacs-root-face ((t (:inherit font-lock-string-face :weight bold))))
   '(treemacs-directory-face ((t (:inherit font-lock-function-name-face))))
   '(treemacs-file-face ((t (:inherit default))))
   '(treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face))))
   '(treemacs-git-added-face ((t (:inherit font-lock-type-face))))
   '(treemacs-git-renamed-face ((t (:inherit font-lock-keyword-face))))
   '(treemacs-git-ignored-face ((t (:inherit shadow)))))
  
  ;; 确保图标正常显示
  (setq treemacs-no-png-images nil))

;; === Hook 设置 ===
(add-hook 'treemacs-mode-hook
          (lambda ()
            ;; 在 treemacs buffer 中禁用一些不必要的 minor modes
            (setq-local truncate-lines t)
            (setq-local word-wrap nil)))

;; === 项目自动添加（可选）===
(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (when (treemacs-get-local-window)
                (treemacs-add-project-to-workspace)))))

(message "Improved Treemacs NeoTree-style configuration loaded! Use SPC e to toggle.")

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

;; Evil Collection 提供各种 mode 的 Vim 按键绑定
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; 现在配置 pdf-tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-continuous t)
  
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode)))
  
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package modus-themes
  :ensure t)


;; Dashboard with Evil mode integration - Simplified

 (use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  ;; (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
			  (bookmarks . 5)  ;; 显示多少个最近书签
			  (projects . 10))) ;; 显示多少个最近项目
  (dashboard-setup-startup-hook))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (lambda (file)
                 (string-match-p "treemacs-persist" file))))

(provide 'init-ui)
;;; init-ui.el ends here
