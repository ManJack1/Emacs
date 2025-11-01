(defun toggle-eat ()
  "Toggle between eat terminal and previous buffer. Create eat terminal if not exists."
  (interactive)
  (let ((eat-buffer (get-buffer "*eat*")))
    (if (and eat-buffer (eq (current-buffer) eat-buffer))
        ;; Currently in eat buffer, switch back to previous
        (if (and (boundp 'eat-previous-buffer) 
                 (buffer-live-p eat-previous-buffer))
            (switch-to-buffer eat-previous-buffer)
          (previous-buffer))
      ;; Store current buffer before switching
      (setq eat-previous-buffer (current-buffer))
      ;; Switch to eat or create if not exists  
      (if eat-buffer
          (switch-to-buffer eat-buffer)
        (eat)))))

;; PDFTools 固定使用 doom-one 配色
(with-eval-after-load 'pdf-view
  ;; 设置 doom-one 风格的颜色
  (setq pdf-view-midnight-colors '("#FFFFFF" . "#282c34"))
  
  ;; 打开 PDF 时自动启用配色
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-midnight-minor-mode 1))))

;; 可选：手动切换开关（如果偶尔需要原始颜色）
(defun pdf-toggle-colors ()
  "Toggle PDF midnight mode on/off."
  (interactive)
  (pdf-view-midnight-minor-mode 'toggle)
  (when (derived-mode-p 'pdf-view-mode)
    (pdf-view-redisplay t)))

(defun my/auto-switch-modus-theme ()
  "根据时间自动切换 Modus 主题"
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (or (>= hour 22) (< hour 6))
        (load-theme 'modus-vivendi-tinted t)
      (load-theme 'modus-operandi-tinted t))))

(defun my/org-math-preview-on-save ()
  "在保存 Org 文件时自动执行 math-preview-all，排除 config.org."
  (when (and (eq major-mode 'org-mode)
             (not (string-equal (file-name-nondirectory (or buffer-file-name "")) "config.org")))
    (math-preview-all)))

;; 打开 Org 文件时执行
(defun my/org-math-preview-on-open ()
  "打开 Org 文件时自动执行 math-preview-all，排除 config.org."
  (when (not (string-equal (file-name-nondirectory (or buffer-file-name "")) "config.org"))
    (math-preview-all)))

(add-hook 'org-mode-hook #'my/org-math-preview-on-open)
(add-hook 'before-save-hook #'my/org-math-preview-on-save)

;; 全局透明度设置
;; 0 = 完全透明，100 = 不透明
(cond
 ;; macOS
 ((eq system-type 'darwin)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100))))
 ;; Linux
 ((eq system-type 'gnu/linux)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100)) ; 活动窗口 90%，非活动窗口 85%
  (add-to-list 'default-frame-alist '(alpha . (100 . 100)))))

(defun toggle-copilot-mode ()
  "Toggle copilot-mode on or off."
  (interactive)
  (if (bound-and-true-p copilot-mode)
      (progn
        (copilot-mode -1)
        (message "Copilot mode disabled"))
    (copilot-mode 1)
    (message "Copilot mode enabled")))

;; 添加到配置中
(with-eval-after-load 'org
  ;; 全局数学公式预览
  (define-key org-mode-map (kbd "C-c C-p")
    (lambda () (interactive) (math-preview-all)))

  ;; 清除所有数学公式预览
  (define-key org-mode-map (kbd "C-c C-r")
    (lambda () (interactive) (math-preview-clear-all))))

(defun my/auto-tangle-config ()
  "自动在保存 config.org 时执行 org-babel-tangle。"
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil)) ;; 不提示确认
      (org-babel-tangle))))

(add-hook 'after-save-hook #'my/auto-tangle-config)

  (defun copy-file-path ()
  "Copy current file absolute path to clipboard."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (progn
          (kill-new (expand-file-name file-path))
          (message "Copied: %s" (expand-file-name file-path)))
      (error "Current buffer is not associated with a file"))))

;;;###autoload
(defun copy-file-name ()
  "Copy current file name to clipboard."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (let ((name (file-name-nondirectory file-path)))
          (kill-new name)
          (message "Copied: %s" name))
      (error "Current buffer is not associated with a file"))))

  ;; LSP imenu 切换函数
  (defun my/toggle-lsp-ui-imenu ()
    "Toggle lsp-ui-imenu sidebar."
    (interactive)
    (if (get-buffer-window "*lsp-ui-imenu*")
        (lsp-ui-imenu--kill)
      (lsp-ui-imenu)))

;; 参数导航（基于 treesitter）
(defun lsp-goto-next-param ()
  "跳转到下一个参数"
  (interactive)
  (when (bound-and-true-p tree-sitter-mode)
    (let ((node (tsc-get-node-at-point (treesit-node-start (treesit-node-at (point))))))
      (when node
        (treesit-search-forward node "parameter_declaration" t)))))

(defun lsp-goto-prev-param ()
  "跳转到前一个参数"
  (interactive)
  (when (bound-and-true-p tree-sitter-mode)
    (let ((node (tsc-get-node-at-point (treesit-node-start (treesit-node-at (point))))))
      (when node
        (treesit-search-backward node "parameter_declaration" t)))))

;; 清空所有 Shift-TAB 绑定
(with-eval-after-load 'company
  (define-key company-active-map (kbd "S-TAB") nil)
  (define-key company-active-map (kbd "<backtab>") nil))

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "S-TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") nil))

;; 清空 company 的 TAB 绑定
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

;; 清空 yasnippet 的 TAB 绑定
(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

;; 定义智能 Shift-TAB：仅 YASnippet 反向跳转
(defun smart-shift-tab ()
  "Shift-TAB：仅用于 YASnippet 反向跳转"
  (interactive)
  (when (and (bound-and-true-p yas-minor-mode)
             (yas-active-snippets))
    (yas-prev-field)))

;; 绑定 Shift-TAB
(global-set-key (kbd "<backtab>") 'smart-shift-tab)

(defun smart-tab ()
  "智能 TAB 键：优先展开/跳转 YASnippet，其次 Copilot，最后正常 TAB。"
  (interactive)
  (cond
   ;; 1. 如果光标在 snippet 缩写词后，尝试展开
   ((and (bound-and-true-p yas-minor-mode)
         (yas-expand)))
   
   ;; 2. 如果有激活的 snippet，占位符跳转
   ((and (bound-and-true-p yas-minor-mode)
         (yas-active-snippets))
    (let ((field (yas--snippet-active-field (car (yas-active-snippets)))))
      (if (and field (yas--field-next field))
          (yas-next-field)
        (yas-exit-all-snippets))))
   
   ;; 3. 如果 Copilot 有建议，接受建议
   ((and (bound-and-true-p copilot-mode)
         (copilot--overlay-visible))
    (copilot-accept-completion))
   
   ;; 4. 否则执行正常的 TAB 缩进
   (t
    (indent-for-tab-command))))

  ;; straight.el 已在 init.el 中初始化
  ;; 这里配置 use-package 的默认行为

  (setq use-package-always-defer t)  ; 延迟加载，提高启动速度

;; 设置编程字体
(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font"
                    :height 155)

;; Set Chinese font for Han script
(set-fontset-font t 'han "Noto Serif CJK SC")

  ;; 关闭不必要的 UI 元素
  (tool-bar-mode -1)        ; 关闭工具栏
  (scroll-bar-mode -1)      ; 关闭滚动条
  (menu-bar-mode -1)        ; 关闭菜单栏

  ;; 启用有用的 UI 功能
  (global-display-line-numbers-mode 1)  ; 显示行号
  (global-visual-line-mode 1)           ; 视觉行模式，软换行
  (global-hl-line-mode 1)               ; 高亮当前行

  ;; 启动配置
  (setq inhibit-startup-message t)      ; 关闭启动画面

(use-package dashboard
  :straight t
  :demand t
  :after (centaur-tabs nerd-icons evil)
  
  :custom
  ;; 基础设置
  (dashboard-banner-logo-title (format "GNU Emacs %s" emacs-version))
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-items-default-length 20)
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  
  ;; 显示项目
  (dashboard-items '((recents . 5) 
                     (bookmarks . 5) 
                     (projects . 5) 
                     (agenda . 5)))
  (dashboard-item-shortcuts '((recents . "r") 
                              (bookmarks . "m") 
                              (agenda . "a") 
                              (projects . "p")))
  
  ;; Agenda
  (dashboard-week-agenda t)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  
  ;; 图标
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  
  ;; Footer
  (dashboard-set-init-info t)
  (dashboard-set-footer t)
  (dashboard-footer-messages '("Happy Hacking!"))
  (dashboard-footer-icon (nerd-icons-faicon "nf-fa-heart" 
                                           :height 1.1 
                                           :v-adjust -0.05 
                                           :face 'error))
  
  :init
  ;; 导航按钮
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-faicon "nf-fa-github" :height 1.1 :v-adjust 0.0)
            "GitHub" "Browse" 
            (lambda (&rest _) (browse-url "https://github.com")))
           
           (,(nerd-icons-octicon "nf-oct-gear" :height 1.1 :v-adjust 0.0)
            "Config" "Edit" 
            (lambda (&rest _) 
              (find-file (expand-file-name "config.org" user-emacs-directory))))
           
           (,(nerd-icons-faicon "nf-fa-refresh" :height 1.1 :v-adjust 0.0)
            "Update" "Packages" 
            (lambda (&rest _) (straight-pull-all))))))
  
  :config
  (setq dashboard-image-banner-max-height 300)
  (setq dashboard-image-banner-max-width 300)
  (setq dashboard-startup-banner "~/.emacs.d/berserk.png")
  (dashboard-setup-startup-hook)
  
  ;; 隐藏 centaur-tabs
  (add-to-list 'centaur-tabs-excluded-prefixes "*dashboard")
  
  ;; Evil 键绑定
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-define-key 'normal dashboard-mode-map
    "r" #'dashboard-jump-to-recents
    "m" #'dashboard-jump-to-bookmarks
    "p" #'dashboard-jump-to-projects
    "a" #'dashboard-jump-to-agenda
    "g" #'dashboard-refresh-buffer
    "q" #'quit-window
    "{" #'dashboard-previous-section
    "}" #'dashboard-next-section
    "j" #'widget-forward
    "k" #'widget-backward
    (kbd "RET") #'widget-button-press))

;; Org-agenda 基础配置
(use-package org
  :defer t
  :init
  (let ((org-dir (expand-file-name "~/org")))
    (unless (file-directory-p org-dir) 
      (make-directory org-dir t)))
  
  :custom
  (org-agenda-files (list (expand-file-name "~/org")))
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 7))

(use-package colorful-mode
  :straight t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings nil)   ;; 全局显示颜色，不仅限字符串
  (css-fontify-colors nil)
  :init
  ;; 每次 buffer 切换或打开都自动启用 colorful-mode
  (add-hook 'after-change-major-mode-hook #'colorful-mode))

(use-package doom-themes
  :straight t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package modus-themes
;;   :straight t
;;   :init
;;   (my/auto-switch-modus-theme)
;;   (run-at-time "00:00" 3600 #'my/auto-switch-modus-theme)
;;   (run-at-time "06:00" 86400 #'my/auto-switch-modus-theme)  ; 每天早上6点
;;   (run-at-time "22:00" 86400 #'my/auto-switch-modus-theme)) ; 每天晚上10点

  (use-package keycast
    :straight t
    :init
    (add-to-list 'global-mode-string '("" keycast-mode-line))
    (keycast-mode-line-mode t))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25         ; 状态栏高度
        doom-modeline-bar-width 3))     ; 左侧条宽度

(use-package centaur-tabs
  :straight t
  :demand t
  :config
  ;; 基础配置
  (setq centaur-tabs-set-bar 'left
        centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-set-modified-marker t)
  
  ;; 计数函数
  (defun my/count-file-buffers ()
    "计算打开的文件 buffer 数量"
    (length (cl-remove-if-not 'buffer-file-name (buffer-list))))
  
  ;; 更新显示
  (defun my/update-tabs-visibility ()
    "2个或以上文件才显示 tabs"
    (let ((count (my/count-file-buffers)))
      (if (>= count 2)
          (unless centaur-tabs-mode (centaur-tabs-mode 1))
        (when centaur-tabs-mode (centaur-tabs-mode -1)))))
  
  ;; 监听 buffer 变化
  (add-hook 'buffer-list-update-hook 'my/update-tabs-visibility)
  
  ;; 初始检查
  (run-with-idle-timer 0.5 nil 'my/update-tabs-visibility))

(use-package indent-bars
  :straight t
  :custom
  (indent-bars-no-descend-lists t)                    ; 列表不显示额外缩进线
  (indent-bars-treesit-support t)                     ; 启用 tree-sitter 支持
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition 
                                       for_statement if_statement 
                                       with_statement while_statement)))
  :hook ((java-ts-mode python-ts-mode yaml-mode c++-ts-mode) . indent-bars-mode))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package nerd-icons
  :straight t
  :defer t
  :if (display-graphic-p))

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :hook (after-init . nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package neotree
  :straight t
  :hook (after-init . (lambda ()
                        (global-set-key [f3] 'neotree-toggle)))
  :config
  (setq neo-window-width 40)  ; 设置固定宽度为30列
  (setq neo-smart-open t)  ; 自动展开到当前文件
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  
  ;; 禁止换行，截断长文件名
  (setq neo-window-fixed-size nil)  ; 允许窗口宽度调整
  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
              (setq truncate-lines t)  ; 截断长行，不换行
              (setq word-wrap nil))))  ; 禁用自动换行

      ;; 编辑体验优化
      (auto-save-visited-mode 1)           ; 自动保存
      (show-paren-mode 1)                  ; 高亮匹配括号
      (global-auto-revert-mode 1)          ; 自动重新加载外部修改的文件
      (delete-selection-mode 1)            ; 选中文字后输入会替换
      (recentf-mode 1)                     ; 最近文件列表

      ;; 启用相对行号
      (global-display-line-numbers-mode 1)
      (setq display-line-numbers-type 'relative)

      ;; 将 yes-or-no-p 替换为 y-or-n-p
      (defalias 'yes-or-no-p 'y-or-n-p)
      ;; 关闭备份文件
      (setq make-backup-files nil)         ; 不创建 ~ 备份文件
      (setq auto-save-default nil)         ; 不创建 # 自动保存文件

(use-package sis
  :straight t
  :init
  ;; macOS 配置
  (when (eq system-type 'darwin)
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.tencent.inputmethod.wetype.pinyin"
     'macOS))
  
  ;; Linux 配置
  (when (eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5))
  
  ;; 启用功能
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)     ; 不用这个，会强制切英文
  (sis-global-context-mode t)        ; 这个会根据上下文智能切换，保持输入法
  (sis-global-inline-mode t))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000                     ;; 保存历史条目数
        savehist-autosave-interval 300         ;; 自动保存间隔（秒）
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))                            ;; 启用 savehist

(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory)))                           ;; 启用 saveplace

(use-package magit
  :config
  ;; 可选配置
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package blamer
  :straight (:host github :repo "artawower/blamer.el")
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background unspecified
                    :height 140
                    :italic t)))
  :init
  (setq blamer-commit-formatter " ● %s")
  (setq blamer-author-formatter "  ✎ %s ")
  (global-blamer-mode 0))

(use-package diff-hl
  :straight t
  :init
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-incompatible-modes
        '(display-line-numbers-mode hl-line-mode visual-line-mode)
        ;; 禁用 pdf-view 的警告
        warning-suppress-types '((pdf-view)))
  :config
  ;; 自动安装 pdf-tools（若 pdf-info 未运行）
  (unless (pdf-info-running-p)
    (pdf-tools-install-noverify))

  ;; 进入 pdf-view-mode 时自动调整显示、关闭不兼容模式
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (hl-line-mode -1)
              (pdf-view-fit-page-to-window))))

(use-package super-save
  :straight t
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t
        super-save-silent t
        auto-save-default nil))

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" 
                         ("e" "e/*") 
                         "*.texi" 
                         "*.ti" 
                         ("terminfo/e" "e/*") 
                         "*.info"))
  :custom
  ;; Use a more compatible terminal type
  (eat-term-name "xterm-256color")  ; or "eterm-color"
  :config
  (server-start) ;;enable emacs open at terminal
  (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode))

(use-package vterm
  :straight t
  :config
  (setq vterm-max-scrollback 10000))

(use-package transient
  :straight t
  :demand t)

  (use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config
  ;; setting-tag
  (which-key-add-key-based-replacements "SPC a" "ai")
  (which-key-add-key-based-replacements "SPC b" "buffer")
  (which-key-add-key-based-replacements "SPC c" "lsp")
  (which-key-add-key-based-replacements "SPC f" "Find-file")
  (which-key-add-key-based-replacements "SPC s" "seach")
  (which-key-add-key-based-replacements "SPC g" "git")
  (which-key-add-key-based-replacements "SPC m" "mark")
  (which-key-add-key-based-replacements "SPC o" "org")
  (which-key-add-key-based-replacements "SPC q" "quit")
  (which-key-add-key-based-replacements "SPC w" "window")
  (which-key-add-key-based-replacements "SPC x" "trouble")
  ;; 快速显示（0.4 秒）
  (setq which-key-idle-delay 0.1)
  ;; 在屏幕底部显示
  (setq which-key-side-window-location 'bottom)
  ;; 显示宽度
  (setq which-key-side-window-max-width 0.5))

  (use-package evil
    :straight t
    :demand t
    :init
    (setq evil-want-keybinding nil)  ; 避免键绑定冲突
    (setq evil-want-C-u-scroll t)    ; C-u 向上滚动
    (setq evil-undo-system 'undo-fu) ; 设置 undo 系统
    :config
    (evil-mode 1)
    ;; 允许 RET 在 org-mode 中跟随链接
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "RET") nil)))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :init
  (evil-commentary-mode))

   (use-package company
     :straight t
     :hook (after-init . global-company-mode)
     :config
  ;; 这个配置会同时显示 yasnippet、LSP、代码词汇 的补全
   (setq company-backends
         '( (company-capf                  ; LSP/完成点
            company-dabbrev-code          ; 代码词汇
            company-files)
           (company-abbrev                ; 缩写（备用）
            company-dabbrev)))            ; 文本词汇（备用）
   
   ;; 启用以下选项以优化多点补全体验
   (setq company-idle-delay 0.2)
   (setq company-minimum-prefix-length 1)
   (setq company-show-quick-access t)
   (setq company-tooltip-align-annotations t)
   
   ;; 允许多个后端同时补全
   (setq company-backend-load-all-backends t)
   
   ;; 显示补全的最大高度
   (setq company-tooltip-limit 20)
   
   ;; 在补全菜单中显示所有后端的候选项
   (setq company-selection-wrap-around t)
   
     :bind (:map company-active-map
                 ("C-n" . company-select-next)
                 ("C-p" . company-select-previous)
                 ("M-<" . company-select-first)
                 ("M->" . company-select-last)
		 ("C-<tab>" . company-complete-common-or-cycle)
                 ;; ("<tab>" . company-complete-selection)
                 ("RET" . company-complete-selection)))

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

  (use-package undo-fu
    :straight t
    :demand t
    :config
    ;; 确保 evil 能找到 undo-fu 的函数
    (global-unset-key (kbd "C-z"))

    ;; 使用内置的 undo 持久化（需要 Emacs 28+）
    (setq undo-no-redo t)
    ;; 增大 undo 限制
    (setq undo-limit 67108864) ; 64mb
    (setq undo-strong-limit 100663296) ; 96mb
    (setq undo-outer-limit 1006632960)) ; 960mb

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :hook (after-init . global-undo-fu-session-mode)
  :config
  ;; 设置会话保存目录
  (setq undo-fu-session-directory 
        (expand-file-name "undo-fu-session/" user-emacs-directory))
  
  ;; 忽略某些文件的撤销历史
  (setq undo-fu-session-incompatible-files
        '("/tmp/" "/dev/shm/" "COMMIT_EDITMSG" ".gpg$"))
  
  ;; 显示 undo-fu-session 的日志信息
  (setq undo-fu-session-linear nil))

(use-package vundo
  :straight t
  :commands (vundo)
  :config
  (setq vundo-compact-display t))

(use-package smartparens
  :straight t
  :hook (after-init . smartparens-global-mode)  ; 全局启用
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always))

(use-package evil-surround
  :straight t
  :after evil
  :config
  ;; 启用全局 evil-surround 模式
  (global-evil-surround-mode 1))

  (use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

  (use-package evil-fringe-mark
    :after evil
    :config
    ;; 方案 1：evil 标记在右 fringe，bookmark 在左 fringe
;; 增加左 fringe 宽度，给 bookmark 图标更多空间
(setq-default left-fringe-width 40)
    (setq-default evil-fringe-mark-side 'right-fringe)

    ;; 可选：调整 evil 标记的样式
    (setq-default evil-fringe-mark-show-special t)  ;; 显示特殊标记
    (global-evil-fringe-mark-mode))

(use-package vertico
  :straight t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-scroll-margin 0)  ; 滚动边距
  (vertico-count 20)         ; 显示 20 个候选项
  (vertico-resize t)         ; 自动调整大小
  (vertico-cycle t))         ; 循环导航

(use-package savehist
  :straight nil  ; 内置包
  :init
  (savehist-mode))

(use-package emacs
  :straight nil
  :custom
  (context-menu-mode t)                    ; 启用上下文菜单
  (enable-recursive-minibuffers t)         ; 允许递归 minibuffer
  (read-extended-command-predicate 
   #'command-completion-default-include-p) ; 只显示可用命令
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode t))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":"))

  (use-package consult-todo
  :straight t
  :commands (consult-todo consult-todo-project))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act)
   ;; ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package wgrep
  :straight t
  :config
  (setq wgrep-change-readonly-file t)
  (setq wgrep-enable-key "e"))
(add-hook 'grep-mode-hook 'wgrep-setup)

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package windmove
  :straight nil)  ; 内置包

  (use-package projectile
    :straight t
    :hook (after-init . projectile-mode)
    :config
    (setq projectile-completion-system 'default))

(use-package avy
  :straight t
  :commands (avy-goto-char avy-goto-char-2 avy-goto-word-1 avy-goto-line)
  :config
  (setq avy-style 'at-full)
  (setq avy-all-windows t))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-c C-t") #'pdf-toggle-colors))

  ;; Normal 模式键位
  (evil-define-key 'normal 'global


    
    ;;org
    (kbd "SPC o p") 'org-latex-preview
    
    ;;terminal
    (kbd "C-/") 'toggle-eat

    ;;git
    (kbd "SPC g g") 'magit
    (kbd "SPC g d") 'magit-diff
    (kbd "SPC g i") 'magit-info
    (kbd "SPC g l") 'magit-log
    (kbd "B") 'blamer-show-commit-info

    ;;ai
    (kbd "SPC c e") 'toggle-copilot-mode
    (kbd "SPC a d") 'gptel-add
    (kbd "SPC a a") 'gptel
    (kbd "SPC a m") 'gptel-menu
    (kbd "SPC a t") 'gptel-tools
    (kbd "SPC g c") 'gptel-commit
    (kbd "SPC c a") 'claude-code-ide
    (kbd "SPC c t") 'claude-code-ide-stop
    (kbd "SPC c r") 'claude-code-ide-resume
    (kbd "SPC c c") 'claude-code-continue
   

    ;; 文件操作
    (kbd "SPC f f") 'find-file
    (kbd "SPC f g") 'consult-fd
    (kbd "C-s") 'save-buffer
    
    ;;undo
    (kbd "u") 'undo-only
    (kbd "C-r") 'undo-fu-only-redo

    ;;avy
    (kbd "s") 'avy-goto-char
    (kbd "S") 'avy-goto-word-1
    
    ;; 窗口管理
    (kbd "SPC -") 'split-window-below    ; 水平分割
    (kbd "SPC |") 'split-window-right   ; 垂直分割（用 \\ 代替 |）
    (kbd "SPC w d") 'delete-window       ; 删除窗口
    (kbd "SPC w k") 'enlarge-window
    (kbd "SPC w j") 'shrink-window
    (kbd "SPC w l") 'enlarge-window-horizontally
    (kbd "SPC w h") 'shrink-window-horizontally
    (kbd "C-h") 'evil-window-left
    (kbd "C-j") 'evil-window-down
    (kbd "C-k") 'evil-window-up
    (kbd "C-l") 'evil-window-right
    
    ;; 缓冲区管理
    (kbd "SPC b d") 'evil-delete-buffer
    
    ;; 标签管理
    (kbd "SPC f t") 'centaur-tabs-switch-group
    (kbd "H") 'centaur-tabs-backward-tab
    (kbd "L") 'centaur-tabs-forward-tab
    
    ;; 文件树
    (kbd "SPC e") 'neotree-toggle
    
    ;; LSP
    (kbd "g d") 'lsp-goto-type-definition
    (kbd "g r") 'lsp-ui-peek-find-references
    (kbd "g i") 'lsp-find-implementation
    (kbd "SPC c s") 'my/toggle-lsp-ui-imenu
    (kbd "SPC c f") 'lsp-format-buffer
    (kbd "SPC c a") 'lsp-execute-code-action
    (kbd "SPC c r") 'lsp-rename
    (kbd "SPC c i") 'describe-mode
    (kbd "K") 'lsp-ui-doc-glance
    
    ;; org-download
    (kbd "SPC i p") 'org-download-clipboard
    (kbd "SPC i d") 'org-download-delete
    
    ;; evil-surround
    (kbd "g s d") 'evil-surround-delete
    (kbd "g s r") 'evil-surround-change
    
    ;; 脚本
    (kbd "SPC c p") 'copy-file-path
    
    ;; bookmark
    (kbd "SPC m s") 'bookmark-set
    (kbd "SPC m j") 'bookmark-jump
    (kbd "SPC m d") 'bookmark-delete
    
    ;; 搜索和导航
    (kbd "SPC s f") 'describe-function

    ;;diagnostics
    (kbd "SPC x x") 'lsp-treemacs-errors-list
    (kbd "[ d") 'flymake-goto-prev-error
    (kbd "] d") 'flymake-goto-next-error
    (kbd "[ t") 'hl-todo-previous
    (kbd "] t") 'hl-todo-next

    ;;quit/session

    (kbd "SPC q q") 'save-buffers-kill-terminal
    (kbd "SPC q Q") 'save-buffers-kill-emacs

    (kbd "SPC f d") 'project-find-dir
    (kbd "SPC f p") 'project-find-file
    (kbd "SPC f P") 'project-switch-project
    (kbd "SPC f r") 'consult-recent-file


    (kbd "SPC s e") 'consult-flymake
    (kbd "SPC s y") 'consult-yank-pop
    (kbd "SPC s k") 'embark-bindings
    (kbd "SPC s t") 'hl-todo-occur
    (kbd "SPC s T") 'hl-todo-rgrep
    (kbd "SPC s b") 'consult-line
    (kbd "SPC SPC") 'consult-buffer
    (kbd "SPC s g") 'consult-ripgrep
    (kbd "SPC s G") 'consult-git-grep
    (kbd "SPC s n") 'yas-visit-snippet-file
    (kbd "SPC s m") 'consult-bookmark
    (kbd "SPC s S") 'imenu
    (kbd "SPC s s") 'consult-imenu)

  ;; Visual 模式键位
  (evil-define-key 'visual 'global
    ;;ai
    (kbd "SPC a r") 'gptel-rewrite
    (kbd "SPC a d") 'gptel-add
    (kbd "SPC a a") 'gptel
    (kbd "SPC a m") 'gptel-menu
    (kbd "SPC a t") 'gptel-tools

    (kbd "C-l") 'evil-end-of-line
    (kbd "C-h") 'evil-beginning-of-line
    (kbd "g s a") 'evil-surround-region)  ; 添加周围符号

  (evil-define-key 'insert 'global
    (kbd "C-h") 'backward-char
    (kbd "C-l") 'forward-char  ; 向右
    (kbd "TAB") 'smart-tab)

(with-eval-after-load 'neotree
  (evil-define-key 'normal neotree-mode-map
    ;; h 折叠当前目录（如果是文件夹且已展开），否则进入上一级
    (kbd "h")
    (lambda ()
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (cond
         ;; 当前节点是目录并已展开 → 折叠
         ((and node (file-directory-p node)
               (neo-buffer--expanded-node-p node))
          (neo-buffer--set-expand node nil)
          (neo-buffer--refresh t))
         ;; 否则回到上级目录
         (t (neotree-select-up-node)))))

    ;; l 打开文件或展开目录
    (kbd "l")
    (lambda ()
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (if (file-directory-p node)
              (progn
                (neo-buffer--set-expand node t)
                (neo-buffer--refresh t)
                (neotree-next-line))
            (neotree-enter)))))

    ;; 其他常用快捷键
    (kbd "q") 'neotree-toggle
    (kbd "a") 'neotree-create-node
    (kbd "d") 'neotree-delete-node
    (kbd "r") 'neotree-rename-node
    (kbd "y") 'neotree-copy-node
    (kbd "RET") 'neotree-enter))

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-with-inline-images t)
  (setq org-image-align 'center)
  (setq org-image-actual-width '(800))
  (setq org-directory "~/org"
        org-startup-indented t
        org-hide-emphasis-markers t)
  
  ;; org-babel 配置
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t)
     (java . t)
     (js . t)
     (ruby . t)
     (perl . t)
     (css . t)
     (latex . t)
     (org . t)))
  
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-highlight-latex-and-related '(latex script entities))
  
  ;; 设置 org 标题的 doom-one 配色和字体大小
  (set-face-attribute 'org-level-1 nil :foreground "#51afef" :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "#c678dd" :height 1.4)
  (set-face-attribute 'org-level-3 nil :foreground "#98be65" :height 1.3)
  (set-face-attribute 'org-level-4 nil :foreground "#da8548" :height 1.2)
  (set-face-attribute 'org-level-5 nil :foreground "#5699af" :height 1.1)
  (set-face-attribute 'org-level-6 nil :foreground "#a9a1e1" :height 1.0))

(use-package auctex
  :straight t
  :defer t
  :config
  ;; 自动保存时重新生成文档
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; 使用 PDF 模式而不是 DVI
  (setq TeX-PDF-mode t)
  ;; 启用 RefTeX 支持
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; 自动折叠环境
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  ;; 自动补全数学符号
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-source-correlate-start-server t))

(use-package math-preview
  :straight (:host gitlab :repo "matsievskiysv/math-preview")
  :config
  (when (eq system-type 'gnu/linux)
    (setq math-preview-command 
          (expand-file-name "~/.npm-global/bin/math-preview")))
  
  (setq math-preview-svg-postprocess-functions '())
  
  ;; 块级公式居中对齐
  (advice-add 'math-preview--process-input :after
    (lambda (&rest _)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get ov 'category) 'math-preview)
          (let* ((begin (overlay-start ov))
                 (end (overlay-end ov))
                 (text (buffer-substring-no-properties begin end))
                 (is-display (or (string-match-p "^\\\\\\[" text)
                                (string-match-p "^\\$\\$" text)))
                 (display (overlay-get ov 'display)))
            (when (and is-display display)
              (let* ((image-spec (if (and (listp display) (listp (cdr display)))
                                    (cadr display)
                                  display))
                     (img-size (condition-case nil
                                  (image-size image-spec t)
                                (error nil)))
                     (img-width (if img-size (car img-size) 400))
                     (window-width (window-body-width nil t))
                     (indent (max 0 (/ (- window-width img-width) 2))))
                (overlay-put ov 'before-string 
                            (propertize " " 'display `(space :width (,indent)))))))))))
  
  ;; 窗口大小变化时自动重新居中
  (defvar-local math-preview--last-width nil)
  
  (defun math-preview--auto-recenter (frame)
    (dolist (window (window-list frame))
      (with-current-buffer (window-buffer window)
        (when (cl-some (lambda (ov) (eq (overlay-get ov 'category) 'math-preview))
                       (overlays-in (point-min) (point-max)))
          (let ((width (window-body-width window t)))
            (unless (equal width math-preview--last-width)
              (setq math-preview--last-width width)
              (with-selected-window window
                (math-preview-clear-all)
                (math-preview-all))))))))
  
  (add-to-list 'window-size-change-functions #'math-preview--auto-recenter)
  
  (math-preview-start-process))

     (defvar my/last-math-state nil
       "记录上一次光标是否在数学环境中")

     (defun my/auto-toggle-math-preview ()
       "光标进入数学环境时清除预览，离开时重新预览所有公式"
       (when (and (eq major-mode 'org-mode)
                  (not (string-equal (file-name-nondirectory (or buffer-file-name "")) "config.org")))
         (let ((in-math (texmathp)))  ; 检查是否在数学环境中
           ;; 只在状态变化时执行操作
           (unless (eq in-math my/last-math-state)
             (if in-math
                 ;; 进入数学环境：清除光标处的预览
                 (math-preview-clear-at-point)
               ;; 离开数学环境：预览所有公式
               (math-preview-all))
             ;; 更新状态
             (setq my/last-math-state in-math)))))

     ;; 将函数添加到 post-command-hook
     (add-hook 'org-mode-hook
               (lambda ()
                 (add-hook 'post-command-hook #'my/auto-toggle-math-preview nil t)))

;; 这些配置需要在 org 完全加载后才能执行
(with-eval-after-load 'org
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(setq exec-path (cons "/opt/homebrew/bin" exec-path))

  ;; Retina 优化 + 自动居中
  (plist-put org-format-latex-options :scale 2.0)
  
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-preview-latex-process-alist
        '((imagemagick
           :programs ("xelatex" "magick")
           :description "pdf > png"
           :message "需要安装 xelatex 和 imagemagick"
           :image-input-type "pdf"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("convert -density 300 -trim %f -quality 100 %O"))))

;; 使用 dvipng 渲染 LaTeX 公式
;; (setq org-preview-latex-default-process 'dvipng)

;; (setq org-preview-latex-process-alist
;;       '((dvipng
;;          :programs ("latex" "dvipng")                  ;; 调用 latex 和 dvipng
;;          :description "DVI > PNG"
;;          :message "需要安装 latex 和 dvipng"
;;          :use-xcolor t                                  ;; 支持公式颜色
;;          :image-input-type "dvi"                        ;; 输入类型为 DVI
;;          :image-output-type "png"                       ;; 输出 PNG
;;          :image-size-adjust (1.0 . 1.0)                 ;; 图片缩放
;;          :latex-compiler ("latex -interaction=nonstopmode -output-directory=%o %f")
;;          :image-converter ("dvipng -D 300 -T tight -o %O %f")))) ;; DVI 转 PNG

;; (setq org-preview-latex-default-process 'dvisvgm)


  ;; 行内图片缩放
  (advice-add 'org--create-inline-image :filter-return
              (lambda (img) (image--set-property img :scale 0.5) img))
  
  ;; LaTeX 预览缩放 + 居中
  (defun my/org-latex-preview-setup (beg end image &optional imagetype)
    "设置 LaTeX 预览:Retina 缩放 + 独立公式居中"
    (let ((ov (car (overlays-at beg))))
      (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
        ;; 设置缩放
        (overlay-put ov 'display
                     `(image :type ,(or (and imagetype (intern imagetype)) 'png)
                            :file ,image :ascent center :scale 0.5))
        ;; 独立成行则居中
        (when (save-excursion
                (goto-char beg)
                (and (looking-back "^[[:space:]]*" (line-beginning-position))
                     (goto-char end)
                     (looking-at "[[:space:]]*$")))
          (overlay-put ov 'line-prefix 
                       `(space :align-to (- center (0.5 . ,(overlay-get ov 'display)))))))))
  
  (advice-add 'org--make-preview-overlay :after #'my/org-latex-preview-setup))

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list
        '("◉" "○" "✸" "✿" "✦" "❀" "➤" "▶"))
  
  (setq org-superstar-item-bullet-alist
        '((?* . ?•) (?+ . ?➤) (?- . ?•)))
  
  (setq org-superstar-checkbox-bullet-alist
        '((?X . "☒") (?? . "☐") (?\  . "☐")))
  
  (setq org-superstar-item-indent-offset 2)
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-use-with-org-bullets t))

(use-package org-bars
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :after org
  :hook (org-mode . org-bars-mode)
  :config
  ;; 星号符号配置
  (setq org-bars-stars '(:empty "◉"
                         :invisible "▶"
                         :visible "▼")))
;; 方案2: 统一的次要颜色
;; (setq org-bars-color-options '(:only-one-color t
;;                                :bar-color "#51afef")))  ;;
  ;; 方法 1: 让 bars 跟随标题颜色（推荐）
  ;; 默认就是这样，bars 会继承各级标题的颜色
 ;; (setq org-bars-color-options nil))

(use-package auctex
  :straight t
  :mode (("\\.tex\\'" . LaTeX-mode)
         ("\\.ltx\\'" . LaTeX-mode)
         ("\\.cls\\'" . LaTeX-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil)
  :config
  (require 'texmathp)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; 使用 minted
(setq org-latex-listings 'minted)

;; 语言映射
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-minted-langs '(emacs-lisp "common-lisp"))
  (add-to-list 'org-latex-minted-langs '(elisp "common-lisp")))

;; minted 选项
(setq org-latex-minted-options
      '(("fontsize" "\\small")
        ("breaklines" "true")
        ("breakanywhere" "true")
        ("bgcolor" "boxback")
        ("frame" "leftline")
        ("framerule" "2pt")
        ("rulecolor" "boxblue")
        ("baselinestretch" "1.2")))

(use-package org-contrib
  :straight t
  :after org)

(use-package org-download
  :straight t
  :after org
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-image-dir "./images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-display-inline-images t)
  (setq org-download-image-attr-list '("#+ATTR_ORG: :width 600")))

(use-package procress
  :straight (:host github :repo "haji-ali/procress")
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

(use-package helpful
  :defer 3
  :bind
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-key] . #'helpful-key)
   ([remap describe-command] . #'helpful-command)
   ([remap describe-symbol] . #'helpful-symbol)
   ("C-h C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function))
  :config
  (add-hook 'helpful-mode-hook 'visual-line-mode))

(use-package laas
  :straight t
  :hook ((LaTeX-mode . laas-mode) (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
		    "qq" "\\quad"
		    "c.." "\cdots"
		    "le" "\\leq"
                    "On" "O(n)"
		    "ra" " \\Rightarrow "
		    "oo"  "\\infty"
		    "int" "\\int"
		    "-oo"  "-\\infty"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
		    ";" "\&"
		    "\\" "\\\\"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum\\limits_{$1}^{$2} $3"))

                    "bmat" (lambda () (interactive)
                            (yas-expand-snippet "\\begin{bmatrix} $1 \\end{bmatrix} $2"))

                    "l(" (lambda () (interactive)
                            (yas-expand-snippet "\\left( $1 \\right"))

                    "xsp" (lambda () (interactive)
                            (yas-expand-snippet "$1^{$2} + $1^{2*$2} + \\dots + $1^{n*$2}"))

                    "xas" (lambda () (interactive)
                            (yas-expand-snippet "$1_{1}$2_{1} + $1_{2}$2_{2} + \\dots + $1_{$3}$2_{$3}"))
                    "ff" (lambda () (interactive)
                            (yas-expand-snippet "\\frac{$1}{$2} $0"))

                    "prod" (lambda () (interactive)
                            (yas-expand-snippet "\\prod\\limits_{${1:i}^{${2:n}}${0:x}"))))

(defun my/simple-absorb (pattern replacement)
  "通用的正则吸取函数，支持 YAS 跳转
PATTERN: 正则表达式模式
REPLACEMENT: 替换字符串，用 %s 表示匹配内容，支持 $1, $2, $0 跳转点"
  (cond
   ;; 1. 尝试正则模式匹配吸取
   ((looking-back pattern (line-beginning-position))
    (let ((matched (match-string 1)))
      (replace-match "")
      (yas-expand-snippet (format replacement matched))))
   ;; 2. 尝试 LAAS 对象包装（如果存在的话）
   ((and (fboundp 'laas-object-on-left-condition)
         (laas-object-on-left-condition)
         (string-match "\\\\\\([a-z]+\\)" replacement))
    (laas-wrap-previous-object (match-string 1 replacement)))
   ;; 3. 普通插入
   (t 
    (yas-expand-snippet (replace-regexp-in-string "%s" "" replacement)))))

;; 核心吸取函数
(defun my/absorb-sub ()
  "下标吸取: xsub -> x_{} 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)sub" "%s_{$1}$0"))


(defun my/absorb-pow ()
  "下标吸取: xpow -> x^{} 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)pow" "%s^{$1}$0"))


(defun my/absorb-brace ()
  "xbc -> x() 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)bc" "%s($0)"))

(defun my/absorb-bb ()
  "黑板体吸取: Abb -> \\mathbb{A}"
  (interactive)
  (my/simple-absorb "\\([A-Z]\\)bb" "\\mathbb{$s}"))


(defun my/absorb-bf ()
  "黑板体吸取: Abb -> \\mathbb{A}"
  (interactive)
  (my/simple-absorb "\\([A-Z]\\)bf" "\\mathbf{$s}"))

(defun my/absorb-hat ()
  "帽子吸取: xhat -> \\hat{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)hat" "\\hat{$s}"))

;; 注册到 LAAS
(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    "sub" #'my/absorb-sub
    "bb" #'my/absorb-bb
    "bf" #'my/absorb-bf
    "pow" #'my/absorb-pow
    "hat" #'my/absorb-hat
    "bc" #'my/absorb-brace))


(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
                    :cond (lambda () (not (texmathp)))
                    "ii" (lambda () (interactive)
                            (yas-expand-snippet "\\\\( $0 \\\\)"))
                    "dd" (lambda () (interactive)
                            (yas-expand-snippet "\\[\n $0 \n\\]"))))

(use-package lsp-mode
  :straight t
  :hook ((c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (python-ts-mode . lsp)
         (nix-ts-mode . lsp)
         (java-ts-mode . lsp)
         (go-mode . lsp))
  :commands lsp
  :init
  ;; 配置 nix-nil 服务器
  (when (eq system-type 'darwin)
    (setq lsp-nix-nil-server-path "/Users/luoyaohui/.nix-profile/bin/nil"))
  
  (when (eq system-type 'gnu/linux)
    (setq lsp-clients-clangd-executable "/etc/profiles/per-user/manjack/bin/clangd")
    (setq lsp-nix-nil-server-path "nil")) ;; Linux 上从 PATH 找
  
  :config
  (setq lsp-prefer-flymake nil))

(add-hook 'lsp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  (use-package lsp-ui
    :straight t
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-sideline-enable t)
    (lsp-ui-doc-position 'at-point)      ; 在光标位置显示
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-diagnostics-provider :flymake)  ; 确保用 flymake
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-always-show t)
    (lsp-ui-peek-show-directory t))

  (use-package sideline
  :straight t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-backends-right '(sideline-flymake)))

  (use-package sideline-flymake
  :straight t
  :after sideline flymake)

  (use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode))

(use-package nix-ts-mode
  :straight t
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . lsp))

  (use-package lsp-pyright
  :straight t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-java
  :straight t
  :after lsp-mode
  :hook (java-ts-mode . lsp))

(use-package treesit-auto
  :straight t
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt))  ; 提示安装语法

;; (use-package ai-code-interface
;;   :straight (:host github :repo "tninja/ai-code-interface.el")
;;   :after claude-code-ide
;;   :bind ("C-c a" . ai-code-menu)
;;   :config
;;   (ai-code-set-backend 'claude-code-ide)
  
;;   ;; 可选：Magit 集成
;;   (with-eval-after-load 'magit
;;     (ai-code-magit-setup-transients)))

(use-package org-sliced-images
  :straight t
  :config
  (org-sliced-images-mode 1))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(defun my-window-resize ()
  "窗口调整模式，连按 h/j/k/l"
  (interactive)
  (message "使用 h/j/k/l 调整窗口大小")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "h" (lambda () (interactive) (shrink-window-horizontally 3) (my-window-resize)))
     (define-key map "j" (lambda () (interactive) (shrink-window 3) (my-window-resize)))
     (define-key map "k" (lambda () (interactive) (enlarge-window 3) (my-window-resize)))
     (define-key map "l" (lambda () (interactive) (enlarge-window-horizontally 3) (my-window-resize)))
     map)))

(global-set-key (kbd "C-c w") 'my-window-resize)

(use-package gptel
  :straight t
   :config
;; OPTIONAL configuration
(setq gptel-default-mode 'org-mode)
(setq gptel-model 'gpt-4o
      gptel-backend (gptel-make-gh-copilot "Copilot"))

(gptel-make-gh-copilot "Copilot"))

(use-package mcp
  :straight t)

(require 'gptel-integrations)

(setq mcp-hub-servers
      '(("github"
         :command "github-mcp-server"
         :args ("stdio"))))

(use-package gptel-commit
  :straight t
  :after (gptel magit)
  :custom
  (gptel-commit-stream t))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  ;; :hook (prog-mode  . copilot-mode)  ; 在编程模式和 org-mode 下自动启用
  :config
  ;; 禁用缩进警告
  (setq copilot-disable-predicates 
        '(copilot-hide-completion-on-escape
          copilot-hide-completion-on-comment
          copilot-hide-completion-on-string))
  (setq copilot-indent-offset-warning-disable t)
  ;; 显示设置
  (setq copilot-enable-predicates '(copilot-hide-completion-on-escape))
  (setq copilot-idle-delay 0.5))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package evil-matchit
  :straight t
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-anzu
  :straight t
  :after evil
  :diminish
  :demand t
  :init
  (global-anzu-mode t))

(use-package kdl-mode
  :straight t)
