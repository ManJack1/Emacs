(defun my-toggle-deer ()
  "Toggle deer mode on/off."
  (interactive)
  (if (derived-mode-p 'ranger-mode)
      (ranger-close)
    (deer)))

(when-let ((zsh-path (executable-find "zsh")))
  (setq explicit-shell-file-name zsh-path)
  (setq shell-file-name zsh-path)
  (setq-default explicit-shell-file-name zsh-path)
  (setq-default shell-file-name zsh-path))

(defun open-in-sioyek ()
  "Open the PDF file of the current buffer in Sioyek."
  (interactive)
  (let ((pdf-file (buffer-file-name)))
    (if (and pdf-file (string-match-p "\\.pdf\\'" pdf-file))
        (start-process "sioyek" nil "sioyek" pdf-file)
      (message "The current buffer is not a PDF file"))))

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
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))
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

;; Tabout 功能：跳出括号、引号等（仅当前行）
(defun my-tabout ()
  "跳出括号、引号、尖括号等配对符号（仅当前行）"
  (interactive)
  (re-search-forward "[])}\"'>]" (line-end-position) t))

;; Tabout 反向功能：跳到前面的左括号、引号等（仅当前行）
(defun my-tabout-backward ()
  "反向跳到括号、引号、尖括号等配对符号的开始（仅当前行）"
  (interactive)
  (when (re-search-backward "[[({\"'<]" (line-beginning-position) t)
    (forward-char 1)
    t))

;; 定义智能 Shift-TAB：YASnippet 反向跳转 + 反向 tabout
(defun smart-shift-tab ()
  "Shift-TAB：优先 YASnippet 反向跳转，其次反向 tabout"
  (interactive)
  (cond
   ;; 1. 如果有激活的 snippet，反向跳转字段
   ((and (bound-and-true-p yas-minor-mode)
         (yas-active-snippets))
    (yas-prev-field))
   
   ;; 2. 反向 tabout：跳到前面的左括号内
   ((my-tabout-backward))
   
   ;; 3. 否则执行默认行为
   (t
    (indent-for-tab-command))))

;; 绑定 Shift-TAB
(global-set-key (kbd "<backtab>") 'smart-shift-tab)

(defun smart-tab ()
  "智能 TAB 键：优先 org 表格，其次展开/跳转 YASnippet，再次 Copilot，然后 tabout，最后正常 TAB。"
  (interactive)
  (cond
   ;; 1. 如果在 org-mode 表格中，使用 org 表格的 TAB 功能
   ((and (derived-mode-p 'org-mode)
         (org-at-table-p))
    (org-table-next-field))
   
   ;; 2. 如果光标在 snippet 缩写词后，尝试展开
   ((and (bound-and-true-p yas-minor-mode)
         (yas-expand)))
   
   ;; 3. 如果有激活的 snippet，占位符跳转
   ((and (bound-and-true-p yas-minor-mode)
         (yas-active-snippets))
    (let ((field (yas--snippet-active-field (car (yas-active-snippets)))))
      (if (and field (yas--field-next field))
          (yas-next-field)
        (yas-exit-all-snippets))))
   
   ;; 4. 如果 Copilot 有建议，接受建议
   ((and (bound-and-true-p copilot-mode)
         (copilot--overlay-visible))
    (copilot-accept-completion))
   
   ;; 5. Tabout：跳出括号、引号等
   ((my-tabout))
   
   ;; 6. 否则执行正常的 TAB 缩进
   (t
    (indent-for-tab-command))))

  ;; straight.el 已在 init.el 中初始化
  ;; 这里配置 use-package 的默认行为

  (setq use-package-always-defer t)  ; 延迟加载，提高启动速度

;; 设置编程字体
(set-face-attribute 'default nil
                    :font "Maple Mono NF"
                    :height 140)

;; Set Chinese font for Han script
(set-fontset-font t 'han "Noto Serif CJK SC")

  ;; 关闭不必要的 UI 元素
  (tool-bar-mode -1)        ; 关闭工具栏
  (scroll-bar-mode -1)      ; 关闭滚动条
  (menu-bar-mode -1)        ; 关闭菜单栏

  ;; 启用有用的 UI 功能
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)

  (global-visual-line-mode t)
  (setq word-wrap t)
  (setq word-wrap-by-category t)
  ;; 启动配置
  (setq inhibit-startup-message t)      ; 关闭启动画面

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  
  :config
  (setq dired-truncate-lines t)
  
  ;; 调整窗口宽度,给更多空间
  (setq dirvish-default-layout '(0.25 0.25 0.5))  ; 左:0 中:30% 右:70%
  ;; 或者
  (setq dirvish-preview-dispatchers nil)  ; 关闭预览,给父窗口更多空间
  
  (setq dirvish-attributes
        '(nerd-icons file-size collapse subtree-state vc-state git-msg)))


(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package dired-subtree
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

(use-package dired
  :straight nil
  :config
  ;; 截断长文件名
  (setq dired-truncate-lines t)
  
  ;; 其他实用设置
  (setq dired-listing-switches "-alh"      ; 人类可读的文件大小
        dired-dwim-target t                 ; 智能猜测复制目标
        dired-kill-when-opening-new-dired-buffer t)  ; 打开新目录时关闭旧 buffer
  
  :hook
  (dired-mode . (lambda ()
                  (setq truncate-lines t)
                  (hl-line-mode 1))))  ; 高亮当前行

(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package dashboard
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
  (let ((org-dir (expand-file-name "~/.emacs.d/org")))
    (unless (file-directory-p org-dir) 
      (make-directory org-dir t)))
  
  :custom
 (org-agenda-custom-commands
        '(("v" "A better agenda view"
            ((tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                    (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (tags "PRIORITY=\"B\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                    (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
            (tags "PRIORITY=\"C\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                    (org-agenda-overriding-header "Low-priority unfinished tasks:")))
            (agenda "")
            (alltodo "")))))
  (org-agenda-files (list (expand-file-name "~/.emacs.d/org")))
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 7))

(use-package markdown-mode
  :commands (gfm-mode markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
  :config
  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)
  
  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit)
        t)))
  
  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))
  
  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  
  :hook
  (markdown-mode . nb/markdown-unhighlight))

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings nil)   ;; 全局显示颜色，不仅限字符串
  (css-fontify-colors nil)
  :init
  ;; 每次 buffer 切换或打开都自动启用 colorful-mode
  (add-hook 'after-change-major-mode-hook #'colorful-mode))

(use-package doom-themes
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'doom-everforest t)
  ;; (setq doom-themes-enable-bold t
  ;;       doom-themes-enable-italic t)
  ;; (load-theme 'doom-one t)

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
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25         ; 状态栏高度
        doom-modeline-bar-width 3))     ; 左侧条宽度

(use-package centaur-tabs
  :init
  ;; 🔑 关键：daemon 模式兼容性修复
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'centaur-tabs-mode)
    (add-hook 'after-init-hook 'centaur-tabs-mode))
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
  :custom
  (indent-bars-no-descend-lists t)                    ; 列表不显示额外缩进线
  (indent-bars-treesit-support t)                     ; 启用 tree-sitter 支持
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition 
                                       for_statement if_statement 
                                       with_statement while_statement)))
  :hook ((kdl-mode nix-ts-mode java-ts-mode python-ts-mode yaml-mode c++-ts-mode) . indent-bars-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :defer t
  :if (display-graphic-p))

(use-package nerd-icons-completion
  :after marginalia
  :hook (after-init . nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package neotree
  :hook (after-init . (lambda ()
                        (global-set-key [f3] 'neotree-toggle)))
  :config
  (setq neo-window-width 40)
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (setq neo-window-fixed-size nil)
  
  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
              (setq truncate-lines t)
              (setq word-wrap nil)
              (setq display-line-numbers nil)  ; 完全禁用行号显示
              (display-line-numbers-mode -1))))  ; 确保关闭行号模式

      ;; 编辑体验优化
      (auto-save-visited-mode 0)           ; 自动保存
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

(use-package beacon
  :init
  (beacon-mode 1))

(use-package magit
  :commands (magit-status magit-dispatch)  ;; 明确指定触发命令
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
  :init
  ;; 在 diff-hl 初始化前再次确保 shell 路径正确
  (when-let ((zsh-path (executable-find "zsh")))
    (setq shell-file-name zsh-path))
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package pdf-tools
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
  (eat-term-name "xterm-256color")

  :config
  (server-start)
  (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode)
  
  
  ;; Evil 模式配置
  (evil-set-initial-state 'eat-mode 'emacs)
  
  ;; eat-mode-hook 配置
  (add-hook 'eat-mode-hook
            (lambda ()
              ;; 取消行号显示
              (display-line-numbers-mode -1)
              
              ;; Evil 模式键绑定
              (evil-local-set-key 'normal (kbd "p") 'eat-yank)
              (evil-local-set-key 'normal (kbd "P") 'eat-yank)
              (local-set-key (kbd "C-S-v") 'eat-yank))))

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package transient
  :defer t)  ;; 延迟加载，magit 等包会自动触发

  (use-package which-key
  :hook (after-init . which-key-mode)
  :config
  ;; setting-tag
  (which-key-add-key-based-replacements "SPC a" "ai")
  (which-key-add-key-based-replacements "SPC b" "buffer")
  (which-key-add-key-based-replacements "SPC c" "lsp")
  (which-key-add-key-based-replacements "SPC f" "Find-file")
  (which-key-add-key-based-replacements "SPC s" "search")
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
  :after evil
  :init
  (evil-collection-init))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-anzu
  :after evil
  :diminish
  :hook (evil-mode . global-anzu-mode))  ;; evil 加载后启用，无需 :demand

(use-package evil-commentary
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
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
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
  :commands (vundo)
  :config
  (setq vundo-compact-display t))

(use-package smartparens
  :hook (after-init . smartparens-global-mode)  ; 全局启用
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always))

(use-package evil-surround
  :after evil
  :config
  ;; 启用全局 evil-surround 模式
  (global-evil-surround-mode 1))

  (use-package expand-region
  :bind ("C-SPC" . er/expand-region))

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :init
  (marginalia-mode t))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":"))

  (use-package consult-todo
  :commands (consult-todo consult-todo-project))

(use-package embark
  :bind
  (("C-;" . embark-act)
   ;; ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package wgrep
  :config
  (setq wgrep-change-readonly-file t)
  (setq wgrep-enable-key "e"))
(add-hook 'grep-mode-hook 'wgrep-setup)

(use-package consult
  :config
  ;; 用 consult 替换默认的 xref 显示
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; === 正常 buffer ===
  (defvar my/consult--source-normal-buffer
    (list :name     "Buffers"  ; 分隔线样式
          :narrow   ?b
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items
          (lambda ()
            (consult--buffer-query
             :sort 'visibility
             :as #'buffer-name
             :predicate
             (lambda (buf)
               (not (string-prefix-p "*" (buffer-name buf))))))))

  ;; === 系统 buffer ===
  (defvar my/consult--source-system-buffer
    (list :name     "System"
          :narrow   ?*
          :category 'buffer
          :face     'font-lock-comment-face
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items
          (lambda ()
            (consult--buffer-query
             :sort 'visibility
             :as #'buffer-name
             :predicate
             (lambda (buf)
               (string-prefix-p "*" (buffer-name buf)))))))

  (setq consult-buffer-sources
        '(my/consult--source-normal-buffer
          my/consult--source-system-buffer
          consult--source-recent-file
          consult--source-bookmark)))

(use-package embark-consult
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
  :commands (avy-goto-char avy-goto-char-2 avy-goto-word-1 avy-goto-line)
  :config
  (setq avy-style 'at-full)
  (setq avy-all-windows t))

(use-package general
  :config
  
  ;; ==================== 创建 Definer ====================
  
  ;; 全局 Leader key (SPC)
  (general-create-definer global-leader
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  
  ;; Normal 模式专用
  (general-create-definer normal-leader
    :states 'normal
    :prefix "SPC")
  
  ;; Visual 模式专用
  (general-create-definer visual-leader
    :states 'visual
    :prefix "SPC")
  
  ;; ==================== Normal 模式键绑定 ====================
  
  ;; 全局 Normal 模式键（不带 SPC 前缀）
  (general-define-key
   :states 'normal
   "u" 'undo-only
   "C-r" 'undo-fu-only-redo
   "s" 'avy-goto-char-timer
   "S" 'avy-goto-word-1
   "B" 'blamer-show-commit-info
   "K" 'lsp-ui-doc-glance
   "H" 'centaur-tabs-backward-tab
   "L" 'centaur-tabs-forward-tab
   
   ;; 窗口导航
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right
   "C-s" 'save-buffer
   "C-/" 'toggle-eat
   
   ;; LSP goto
   "g d" 'lsp-goto-type-definition
   "g r" 'lsp-ui-peek-find-references
   "g i" 'lsp-find-implementation
   "g s d" 'evil-surround-delete
   "g s r" 'evil-surround-change
   
   ;; Diagnostics navigation
   "[ d" 'flymake-goto-prev-error
   "] d" 'flymake-goto-next-error
   "[ t" 'hl-todo-previous
   "] t" 'hl-todo-next)
  
  ;; 带 SPC 前缀的 Normal 模式键
  (normal-leader
    ;; 快速访问
    "SPC" 'consult-buffer
    
    ;; Org (o)
    "o" '(:ignore t :which-key "org")
    "op" 'org-latex-preview
    
    ;; Git (g)
    "g" '(:ignore t :which-key "git")
    "gg" 'magit
    "gd" 'magit-diff
    "gi" 'magit-info
    "gl" 'magit-log
    "gc" 'gptel-commit
    
    ;; AI (a & c)
    "a" '(:ignore t :which-key "ai")
    "ad" 'gptel-add
    "aa" 'gptel
    "am" 'gptel-menu
    "at" 'gptel-tools
    
    "c" '(:ignore t :which-key "code/ai")
    "ce" 'toggle-copilot-mode
    "ca" 'claude-code-ide
    "ct" 'claude-code-ide-stop
    "cr" 'claude-code-ide-resume
    "cc" 'claude-code-continue
    "cs" 'my/toggle-lsp-ui-imenu
    "cf" 'lsp-format-buffer
    "ca" 'lsp-execute-code-action
    "cr" 'lsp-rename
    "ci" 'describe-mode
    "cp" 'copy-file-path

    "ot" 'org-todo
    "oa" 'org-agenda
    "oc" 'org-capture
    "oT" 'org-tags-view
    
    ;; File (f)
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "fg" 'consult-fd
    "fn" 'consult-notes
    "fz" 'consult-dir
    "ft" 'centaur-tabs-switch-group
    "fd" 'project-find-dir
    "fp" 'project-find-file
    "fP" 'project-switch-project
    "fr" 'consult-recent-file
    
    ;; Buffer (b)
    "b" '(:ignore t :which-key "buffer")
    "bd" 'evil-delete-buffer
    
    ;; Window (w)
    "w" '(:ignore t :which-key "window")
    "-" 'split-window-below
    "|" 'split-window-right
    "wd" 'delete-window
    "wk" 'enlarge-window
    "wj" 'shrink-window
    "wl" 'enlarge-window-horizontally
    "wh" 'shrink-window-horizontally
    
    ;; File tree (e)
    "e" 'neotree-toggle
    "d" 'dirvish
    
    ;; Image (i)
    "i" '(:ignore t :which-key "image")
    "ip" 'org-download-clipboard
    "id" 'org-download-delete
    
    ;; Bookmark (m)
    "m" '(:ignore t :which-key "bookmark")
    "ms" 'bookmark-set
    "mj" 'bookmark-jump
    "md" 'bookmark-delete
    
    ;; Search (s)
    "s" '(:ignore t :which-key "search")
    "sf" 'describe-function
    "se" 'consult-flymake
    "sy" 'consult-yank-pop
    "sk" 'embark-bindings
    "st" 'hl-todo-occur
    "sT" 'hl-todo-rgrep
    "sb" 'consult-line
    "sg" 'consult-ripgrep
    "sG" 'consult-git-grep
    "sn" 'yas-visit-snippet-file
    "sm" 'consult-bookmark
    "sh" 'helpful-at-point
    "sS" 'imenu
    "ss" 'consult-imenu
    
    ;; Diagnostics (x)
    "x" '(:ignore t :which-key "diagnostics")
    "xx" 'lsp-treemacs-errors-list
    
    ;; Quit (q)
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qQ" 'save-buffers-kill-emacs)
  
  ;; ==================== Visual 模式键绑定 ====================
  
  (general-define-key
   :states 'visual
   "C-l" 'evil-end-of-line
   "C-h" 'evil-beginning-of-line
   "g s a" 'evil-surround-region)
  
  (visual-leader
    ;; AI
    "a" '(:ignore t :which-key "ai")
    "ar" 'gptel-rewrite
    "ad" 'gptel-add
    "aa" 'gptel
    "am" 'gptel-menu
    "at" 'gptel-tools)
  
  ;; ==================== Insert 模式键绑定 ====================
  
  (general-define-key
   :states 'insert
   "C-/" 'toggle-eat
   "C-h" 'backward-char
   "C-l" 'forward-char
   "TAB" 'smart-tab)
  
  ;; ==================== PDF View 模式 ====================
  
  (general-define-key
   :keymaps 'pdf-view-mode-map
   "C-c C-t" 'pdf-toggle-colors)
  
  ;; ==================== Neotree 模式 ====================
  
  (defun my/neotree-collapse-or-up ()
    "折叠当前目录或进入上级目录."
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
  
  (defun my/neotree-open-or-expand ()
    "打开文件或展开目录."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (if (file-directory-p node)
            (progn
              (neo-buffer--set-expand node t)
              (neo-buffer--refresh t)
              (neotree-next-line))
          (neotree-enter)))))
  
  (with-eval-after-load 'neotree
  (general-define-key
   :states 'normal
   :keymaps 'neotree-mode-map
   "h" 'my/neotree-collapse-or-up
   "l" 'my/neotree-open-or-expand
   "q" 'neotree-toggle
   "a" 'neotree-create-node
   "d" 'neotree-delete-node
   "r" 'neotree-rename-node
   "o" 'dired-jump
   "y" 'neotree-copy-node
   "RET" 'neotree-enter)))

;; ==================== Dirvish 模式 ====================

(with-eval-after-load 'dirvish
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "h" 'dired-up-directory
   "l" 'dired-find-file
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "q" 'dirvish-quit
   "Q" 'kill-current-buffer
   "zh" 'dired-omit-mode
   "TAB" 'dirvish-subtree-toggle
   "gg" 'beginning-of-buffer
   "G" 'end-of-buffer
   "RET" 'dired-find-file))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; 基础设置
  (setq org-directory "~/.emacs.d/org/"
        org-agenda-files '("~/.emacs.d/org/")  ; 扫描整个 org 目录
        org-startup-indented t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-align 'center
        org-image-actual-width '(800))
  
  ;; TODO 配置
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "Buy(b)" "Plan(p)" "Book(k)"
                  "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6c6b" :weight bold))
        ("DOING" . (:foreground "#ECBE7B" :weight bold))
        ("WAITING" . (:foreground "#da8548" :weight bold))
        ("DONE" . (:foreground "#98be65" :weight bold))
        ("CANCELLED" . (:foreground "#5B6268" :weight bold))
        ("Buy" . (:foreground "#51afef" :weight bold))   ; 蓝色，表示行动或采购任务
        ("Book" . (:foreground "#51afef" :weight bold))   ; 蓝色，表示行动或采购任务
        ("Plan" . (:foreground "#c678dd" :weight bold)))) ; 紫色，表示规划/学习

  (require 'org-checklist)
  ;; need repeat task and properties
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  
  ;; 日志设置
  (setq org-log-done 'time
        org-log-into-drawer t)
  
  ;; 优先级
  (setq org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#ECBE7B" :weight bold))
          (?C . (:foreground "#51afef" :weight bold)))
	org-aenda-block-separator 8411)
  
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
  
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)  ; 添加这个
  
  ;; LaTeX 高亮
  (setq org-highlight-latex-and-related '(latex script entities))
  
  ;; 标题字体和颜色（doom-one 风格）
  ;; (set-face-attribute 'org-level-1 nil 
  ;;                     :foreground "#51afef" 
  ;;                     :height 1.0 
  ;;                     :weight 'bold)
  ;; (set-face-attribute 'org-level-2 nil 
  ;;                     :foreground "#c678dd" 
  ;;                     :height 1.0)
  ;; (set-face-attribute 'org-level-3 nil 
  ;;                     :foreground "#98be65" 
  ;;                     :height 1.0)
  ;; (set-face-attribute 'org-level-4 nil 
  ;;                     :foreground "#da8548" 
  ;;                     :height 1.0)
  ;; (set-face-attribute 'org-level-5 nil 
  ;;                     :foreground "#5699af" 
  ;;                     :height 1.0)
  ;; (set-face-attribute 'org-level-6 nil 
  ;;                     :foreground "#a9a1e1" 
  ;;                     :height 1.0)
  
  ;; Agenda 视图设置
  (setq org-agenda-span 7
        org-agenda-start-on-weekday nil
        org-agenda-start-day nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)
  
  ;; Capture 模板
 (setq org-capture-templates
 '(("t" "Todo" entry
    (file+headline "~/.emacs.d/org/todo.org" "  TASK")
    "** TODO %?\nSCHEDULED: %t\n%i\n")

    ("p" "Plan" entry
    (file+headline "~/.emacs.d/org/plan.org" "a PLAN")
    "** Plan %?\nCREATED: %U\n%i\n")

    ("k" "Book" entry
    (file+headline "~/.emacs.d/org/book.org" "  BOOK")
    "** Book ?\nCREATED: %U\n%i\n")

    ("b" "Buy" entry
    (file+headline "~/.emacs.d/org/buy.org" "  SHOP")
    "** Buy %?\nCREATED: %U\n%i\n"))))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("󰯬 " "󰯯 " "󰯲 ")))

(use-package auctex
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

(use-package org-sliced-images
  :after org
  :config
  ;; 启用全局模式
  (org-sliced-images-mode 1)
  
  ;; 自定义选项
  ;; 如果使用 org-indent-mode 或行号，建议启用这个
  (setq org-sliced-images-round-image-height t)
  
  ;; 可选：自动清理占位符行
  (setq org-sliced-images-consume-dummies t))

(use-package org-superstar
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

;; (use-package org-bars
;;   :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
;;   :after org
;;   :hook (org-mode . org-bars-mode)
;;   :config
;;   ;; 星号符号配置
;;   (setq org-bars-stars '(:empty "◉"
;;                          :invisible "▶"
;;                          :visible "▼")))
;; 方案2: 统一的次要颜色
;; (setq org-bars-color-options '(:only-one-color t
;;                                :bar-color "#51afef")))  ;;
  ;; 方法 1: 让 bars 跟随标题颜色（推荐）
  ;; 默认就是这样，bars 会继承各级标题的颜色
 ;; (setq org-bars-color-options nil))

(use-package auctex
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
  :after org)

(use-package org-download
  :after org
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-image-dir "./images"
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-display-inline-images t
        org-download-image-attr-list '("#+ATTR_ORG: :width 600"))
  
  (setq org-download-screenshot-method
        (cond ((eq system-type 'darwin) "screencapture -i %s")
              ((and (eq system-type 'gnu/linux) (getenv "WAYLAND_DISPLAY"))
               "grim -g \"$(slurp)\" %s")
              (t "import %s")))
  
  ;; Wayland 剪贴板
  (when (and (eq system-type 'gnu/linux) (getenv "WAYLAND_DISPLAY"))
    (defun org-download-clipboard ()
      (interactive)
      (let* ((base (or (and (buffer-file-name) 
                            (file-name-directory (buffer-file-name)))
                       default-directory
                       "~/"))
             (dir (expand-file-name "images" base))
             (name (format-time-string (concat org-download-timestamp "screenshot.png")))
             (file (expand-file-name name dir)))
        (make-directory dir t)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (call-process "wl-paste" nil t nil "--type" "image/png")
          (write-region nil nil file nil 'silent))
        (insert (format "#+DOWNLOADED: clipboard @ %s\n#+ATTR_ORG: :width 600\n[[file:./images/%s]]\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S") name))
        (org-display-inline-images t t)))))

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
  :hook ((LaTeX-mode . laas-mode) (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
		    "qq" "\\quad"
		    "c.." "\\cdots"
		    "le" "\\leq"
                    "On" "O(n)"
		    "ra" " \\Rightarrow "
		    "oo"  "\\infty"
		    "int" "\\int"
		    "-oo"  "-\\infty"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
		    ";;" "\&"
		    "\\" "\\\\"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!

                    "bc" (lambda () (interactive)
                            (yas-expand-snippet "($1)$0"))

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
    "hat" #'my/absorb-hat))


(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
                    :cond (lambda () (not (texmathp)))
                    "ii" (lambda () (interactive)
                            (yas-expand-snippet "\\\\( $1 \\\\) $0"))
                    "dd" (lambda () (interactive)
                            (yas-expand-snippet "\\[\n $1 \n\\] $0"))))

(use-package lsp-mode
  :hook ((c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (python-ts-mode . lsp)
         (nix-ts-mode . lsp)
         ;; (java-ts-mode . lsp)
         (go-mode . lsp))
  :commands lsp
  :init
  (when (eq system-type 'darwin)
    (setq lsp-nix-nil-server-path "/Users/luoyaohui/.nix-profile/bin/nil"))
  :config
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-format-on-save nil))

(defcustom my/warning-suppress-message-regexps '(".*semgrep/rulesRefreshed.*")
  "List of warning messages to suppress.
See also `warning-suppress-log-types'."
  :type '(repeat string)
  :group 'my-configuration)

(defun my/display-warning-advise (type message &optional level buffer-name)
  "Allow filtering Emacs warning messages using regexp.
See the varibale `my/warning-suppress-message-regexps'."
  (catch 'exit
    (dolist (regexp my/warning-suppress-message-regexps)
      (when (string-match-p regexp message)
        (throw 'exit nil)))
    (throw 'exit t)))

(add-function :before-while (symbol-function 'display-warning) #'my/display-warning-advise)

(use-package eglot
  :hook (java-ts-mode . eglot-ensure)
  :config
  ;; 添加 jdtls 配置
  (add-to-list 'eglot-server-programs
               '(java-ts-mode . ("jdtls"))))

;; 只在 java-ts-mode 下绑定键位
(general-define-key
 :states 'normal
 :keymaps 'java-ts-mode-map
 :prefix "SPC c"
 "r" #'eglot-rename
 "a" #'eglot-code-actions)

(use-package consult-eglot
  :after (consult eglot))

(use-package apheleia
  :init
  (apheleia-global-mode +1)
  :config
  ;; 启用调试
  (setq apheleia-log-only-errors nil)
  (setq apheleia-log-debug-info t)
  ;; 增加超时时间(默认是 10 秒)
  (setq apheleia-remote-algorithm 'cancel)
  
  ;; LSP 格式化函数
  (cl-defun my/apheleia-lsp-format
      (&key buffer scratch callback &allow-other-keys)
    "Format BUFFER using LSP."
    (with-current-buffer buffer
      (if (and (bound-and-true-p lsp-mode)
               (lsp-feature? "textDocument/formatting"))
          (condition-case err
              (progn
                (lsp-format-buffer)
                (with-current-buffer scratch
                  (erase-buffer)
                  (insert-buffer-substring buffer))
                (funcall callback))
            (error
             (message "LSP format error: %S" err)
             (with-current-buffer scratch
               (erase-buffer)
               (insert-buffer-substring buffer))
             (funcall callback)))
        (with-current-buffer scratch
          (erase-buffer)
          (insert-buffer-substring buffer))
        (funcall callback))))
  
  ;; 注册所有格式化器（使用 push 或 setf）
  (setf (alist-get 'lsp apheleia-formatters)
        'my/apheleia-lsp-format)
  
  ;; 注意：kdlfmt 使用命令列表格式
  (setf (alist-get 'kdlfmt apheleia-formatters)
      '("kdlfmt" "format" "--stdin"))
  (setf (alist-get 'google-java-format apheleia-formatters)
      '("google-java-format" "-"))  
  
  (setf (alist-get 'nixpkgs-fmt apheleia-formatters)
        '("nixpkgs-fmt"))

  ;; 配置模式关联
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'kdl-mode apheleia-mode-alist) 'kdlfmt))

  (use-package lsp-ui
    :straight t
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-sideline-enable t)
    (lsp-ui-doc-position 'at-point)      ; 在光标位置显示
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-show-diagnostics 0)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-diagnostics-provider :flymake)  ; 确保用 flymake
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-always-show t)
    (lsp-ui-peek-show-directory t))


;; 4. Java 模式专用优化
(add-hook 'java-ts-mode-hook
          (lambda ()
            (setq-local apheleia-mode nil)))

  (use-package sideline
  :hook (flymake-mode . sideline-mode))

;; eldoc-box
(use-package eldoc-box
  :config
  (setq eldoc-box-only-multi-line t
        eldoc-box-clear-with-C-g t
        eldoc-box-max-pixel-width 800
        eldoc-box-max-pixel-height 800)
  
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   :prefix "SPC c"
   "h" #'eldoc-box-help-at-point
   "k" #'eldoc-box-scroll-up
   "j" #'eldoc-box-scroll-down))

  (use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode))

(use-package flyover
  :hook ((flycheck-mode . flyover-mode)
         (flymake-mode . flyover-mode))
  :custom
  ;; Checker settings
  (flyover-checkers '(flycheck flymake))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icons
  (flyover-info-icon "")
  (flyover-warning-icon "")
  (flyover-error-icon "")

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2))

(use-package kdl-mode
  :straight t)

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . lsp))

  (use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
(setq lsp-java-vmargs
        '("-noverify"
          "-Xmx4G"
          "-Xms1G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          "-XX:MaxMetaspaceSize=512m"
          "-XX:+ParallelRefProcEnabled"
          "-XX:+DisableExplicitGC")))

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt))  ; 提示安装语法

(use-package quickrun
  :straight t)

;; (use-package ai-code-interface
;;   :straight (:host github :repo "tninja/ai-code-interface.el")
;;   :after claude-code-ide
;;   :bind ("C-c a" . ai-code-menu)
;;   :config
;;   (ai-code-set-backend 'claude-code-ide)
  
;;   ;; 可选：Magit 集成
;;   (with-eval-after-load 'magit
;;     (ai-code-magit-setup-transients)))

(use-package gptel
   :config
;; OPTIONAL configuration
;; (setq gptel-default-mode 'org-mode)
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

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :init
  ;; 设置番茄钟时长
  (setq pomidor-seconds (* 55 60))          ; 25分钟工作时间
  (setq pomidor-break-seconds (* 5 60))     ; 5分钟短休息
  (setq pomidor-long-break-seconds (* 20 60)) ; 20分钟长休息
  (setq pomidor-breaks-before-long 3)       ; 4个短休息后进入长休息
  
  ;; 使用 emacs state,保持原始按键
  (with-eval-after-load 'evil
    (evil-set-initial-state 'pomidor-mode 'emacs))
  
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1)
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          (set-window-buffer nil (current-buffer)))))

;; ──────────────────────────────────────────────────────────────────────
;; 1. general + 定义 global-definer（必须最先）
;; ──────────────────────────────────────────────────────────────────────
(use-package general
  :demand t
  :config
  (general-create-definer global-definer
    :states '(normal visual)  ; 只在 normal 和 visual 模式
    :keymaps 'override
    :prefix "SPC"))
;; ──────────────────────────────────────────────────────────────────────
;; 2. 多光标
;; ──────────────────────────────────────────────────────────────────────
(use-package iedit
  :ensure t
  :init
  (setq iedit-toggle-key-default nil)
  :config
  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))
(use-package evil-multiedit
  :ensure t
  :commands (evil-multiedit-default-keybinds)
  :init
  (evil-multiedit-default-keybinds))
;; ──────────────────────────────────────────────────────────────────────
;; 3. expand-region
;; ──────────────────────────────────────────────────────────────────────
(use-package expand-region
  :config
  (defadvice er/prepare-for-more-expansions-internal
      (around helm-ag/prepare-for-more-expansions-internal activate)
    ad-do-it
    (let ((new-msg (concat (car ad-return-value)
                           ", H to highlight in buffers"
                           ", / to search in project, "
                           "e iedit mode in functions"
                           "f to search in files, "
                           "b to search in opened buffers"))
          (new-bindings (cdr ad-return-value)))
      (cl-pushnew
       '("H" (lambda ()
               (interactive)
               (call-interactively 'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (interactive)
               (call-interactively 'my/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
       '("e" (lambda ()
               (interactive)
               (call-interactively 'evil-multiedit-match-all)))
       new-bindings)
      (cl-pushnew
       '("f" (lambda ()
               (interactive)
               (call-interactively 'my/er-find-file-with-text)))
       new-bindings)
      (cl-pushnew
       '("b" (lambda ()
               (interactive)
               (call-interactively 'my/er-consult-line-with-text)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings)))))


(defun my/er-find-file-with-text ()
  "在 expand-region 中使用选中的文本查找文件"
  (interactive)
  (let ((text (when (use-region-p)
               (buffer-substring-no-properties
                (region-beginning) (region-end)))))
    (deactivate-mark)
    (if text
        (find-file (read-file-name 
                   (format "Find file [%s]: " text)
                   nil nil nil text))
      (call-interactively 'find-file))))

(defun my/er-consult-line-with-text ()
  "在 expand-region 中使用选中的文本搜索缓冲区"
  (interactive)
  (let ((text (when (use-region-p)
               (buffer-substring-no-properties
                (region-beginning) (region-end)))))
    (deactivate-mark)
    (if text
        (consult-line text)
      (call-interactively 'consult-line))))

;; ──────────────────────────────────────────────────────────────────────
;; 4. 搜索函数
;; ──────────────────────────────────────────────────────────────────────
;;;###autoload
(defun my/search-project-for-symbol-at-point ()
  (interactive)
  (if (use-region-p)
      (progn
        (consult-ripgrep (project-root (project-current))
                         (buffer-substring (region-beginning) (region-end))))))
;; ──────────────────────────────────────────────────────────────────────
;; 5. 全局快捷键（现在 global-definer 已定义）
;; ──────────────────────────────────────────────────────────────────────
(global-definer
  "h c" 'zilongshanren/clearn-highlight   ; SPC h c
  "h H" 'zilongshanren/highlight-dwim     ; SPC h H
  "v" 'er/expand-region)                   ; SPC v
;; ──────────────────────────────────────────────────────────────────────
;; 6. 快速替换
;; ──────────────────────────────────────────────────────────────────────
(defun zilongshanren/evil-quick-replace (beg end)
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))
(define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)
;; ──────────────────────────────────────────────────────────────────────
;; 7. highlight-global（用 straight 安装，无 quelpa）
;; ──────────────────────────────────────────────────────────────────────
(use-package highlight-global
  :straight (highlight-global
             :type git
             :host github
             :repo "glen-dai/highlight-global")
  :ensure nil
  :commands (highlight-frame-toggle)
  :config
  (progn
    (setq-default highlight-faces
                  '(('hi-red-b . 0)
                    ('hi-aquamarine . 0)
                    ('hi-pink . 0)
                    ('hi-blue-b . 0)))))
;; ──────────────────────────────────────────────────────────────────────
;; 8. 高亮函数 + symbol-overlay
;; ──────────────────────────────────────────────────────────────────────
(defun zilongshanren/highlight-dwim ()
  (interactive)
  (if (use-region-p)
      (progn
        (highlight-frame-toggle)
        (deactivate-mark))
    (symbol-overlay-put)))
(defun zilongshanren/clearn-highlight ()
  (interactive)
  (clear-highlight-frame)
  (symbol-overlay-remove-all))
(use-package symbol-overlay
  :config
  (define-key symbol-overlay-map (kbd "h") 'nil))

(defun my/setup-frame (frame)
  (with-selected-frame frame
    (set-face-attribute 'default nil
                        :font "Maple Mono NF"
                        :height 140)

    ;; Set Chinese font for Han script
    (set-fontset-font t 'han "Noto Serif CJK SC")
    (load-theme 'doom-everforest t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-frame)
  (my/setup-frame (selected-frame)))

(use-package consult-dir
  :defer t
  :bind ((:map vertico-map
          ("C-x C-d" . consult-dir)))
  :config
  (setq consult-dir-default-command #'consult-dir-dired)

  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))

  (defvar consult-dir--source-zoxide
    `(:name "zoxide"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,(lambda () (executable-find "zoxide"))
      :items ,#'consult-dir--zoxide-dirs)
    "zoxide directory source for `consult-dir'.")

  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package denote
  :config
  ;; Your Denote configuration here
  (setq denote-directory (expand-file-name "~/workspace/notes/")))

(use-package consult-notes
  :straight (:type git
             :host github
             :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  ;; 设置笔记目录
  (setq consult-notes-file-dir-sources 
        '(("Notes" ?n "~/workspace/notes/")))
  
  ;; 如果使用 denote
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  
  ;; 只搜索 denote 目录中的文本文件
  (setq consult-notes-denote-files-function 
        (lambda () (denote-directory-files nil t t))))
