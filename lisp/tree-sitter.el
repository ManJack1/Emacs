;; 原生 Tree-sitter 配置 - 充分利用 Emacs 内置功能
;; 适用于 Emacs 29+，无需任何第三方包

;; =============================================================================
;; 1. 验证 Tree-sitter 支持
;; =============================================================================

(unless (treesit-available-p)
  (error "此配置需要 Emacs 29+ 且编译时启用了 tree-sitter 支持"))

;; =============================================================================
;; 2. 语法文件源配置
;; =============================================================================

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; =============================================================================
;; 3. 自动模式映射 - 优先使用 tree-sitter 版本
;; =============================================================================

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (python-mode . python-ts-mode)
        (java-mode . java-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)))

;; =============================================================================
;; 4. 文件关联 - 确保正确的模式被激活
;; =============================================================================

;; C/C++
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-ts-mode))

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-ts-mode))

;; JavaScript/TypeScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; 其他格式
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

;; =============================================================================
;; 5. 语法高亮级别设置
;; =============================================================================

;; 设置最高级别的语法高亮（0-4，4 为最高）
(setq treesit-font-lock-level 4)

;; =============================================================================
;; 6. 语法安装和管理函数
;; =============================================================================

(defun my/install-treesit-grammars ()
  "安装所有配置的 tree-sitter 语法文件"
  (interactive)
  (let ((installed 0)
        (failed 0))
    (message "开始安装 tree-sitter 语法文件...")
    (dolist (lang-config treesit-language-source-alist)
      (let ((lang (car lang-config)))
        (condition-case err
            (if (treesit-language-available-p lang)
                (message "✓ %s 语法已存在" lang)
              (progn
                (message "正在安装 %s 语法..." lang)
                (treesit-install-language-grammar lang)
                (setq installed (1+ installed))
                (message "✓ %s 语法安装成功" lang)))
          (error
           (setq failed (1+ failed))
           (message "✗ %s 语法安装失败: %s" lang (error-message-string err))))))
    (message "安装完成! 成功: %d, 失败: %d" installed failed)))

(defun my/check-treesit-grammars ()
  "检查已安装的语法文件状态"
  (interactive)
  (let ((available '())
        (missing '()))
    (dolist (lang-config treesit-language-source-alist)
      (let ((lang (car lang-config)))
        (if (treesit-language-available-p lang)
            (push lang available)
          (push lang missing))))
    (message "Tree-sitter 状态:")
    (message "✓ 已安装 (%d): %s" (length available) 
             (mapconcat 'symbol-name (reverse available) ", "))
    (when missing
      (message "✗ 未安装 (%d): %s" (length missing)
               (mapconcat 'symbol-name (reverse missing) ", ")))))

(defun my/reinstall-grammar (lang)
  "重新安装指定语言的语法文件"
  (interactive 
   (list (intern (completing-read 
                  "选择要重新安装的语言: " 
                  (mapcar (lambda (x) (symbol-name (car x))) 
                          treesit-language-source-alist)))))
  (message "重新安装 %s 语法..." lang)
  (treesit-install-language-grammar lang)
  (if (treesit-language-available-p lang)
      (message "✓ %s 语法重新安装成功" lang)
    (message "✗ %s 语法重新安装失败" lang)))

;; =============================================================================
;; 7. 编程模式增强配置
;; =============================================================================

;; 通用编程环境设置
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; 缩进和制表符设置
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; C/C++ 特定设置
(defun my/c-ts-mode-setup ()
  "C/C++ tree-sitter 模式设置"
  (setq c-ts-mode-indent-offset 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))

(add-hook 'c-ts-mode-hook 'my/c-ts-mode-setup)
(add-hook 'c++-ts-mode-hook 'my/c-ts-mode-setup)

;; Python 设置
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq python-indent-offset 4)
            (setq indent-tabs-mode nil)))

;; JavaScript/TypeScript 设置
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq indent-tabs-mode nil)))

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq typescript-ts-mode-indent-offset 2)
            (setq indent-tabs-mode nil)))

;; =============================================================================
;; 8. 实用调试和探索功能
;; =============================================================================

(defun my/treesit-explore-current-buffer ()
  "在当前缓冲区中启用 tree-sitter 探索模式"
  (interactive)
  (if (treesit-parser-list)
      (treesit-explore-mode)
    (message "当前缓冲区没有激活的 tree-sitter 解析器")))

(defun my/treesit-info ()
  "显示当前缓冲区的 tree-sitter 信息"
  (interactive)
  (let ((parsers (treesit-parser-list)))
    (if parsers
        (progn
          (message "当前模式: %s" major-mode)
          (message "激活的解析器: %s" 
                   (mapconcat (lambda (p) (symbol-name (treesit-parser-language p))) 
                              parsers ", "))
          (message "语法高亮级别: %d" treesit-font-lock-level))
      (message "当前缓冲区未使用 tree-sitter"))))

(defun my/toggle-treesit-inspect-mode ()
  "切换 tree-sitter 检查模式"
  (interactive)
  (if (bound-and-true-p treesit-inspect-mode)
      (treesit-inspect-mode -1)
    (treesit-inspect-mode 1)))

;; =============================================================================
;; 9. 按键绑定
;; =============================================================================

;; 创建一个 tree-sitter 专用的按键前缀
(define-prefix-command 'my-treesit-map)
(global-set-key (kbd "C-c t") 'my-treesit-map)

;; 绑定具体功能
(define-key my-treesit-map (kbd "i") 'my/install-treesit-grammars)
(define-key my-treesit-map (kbd "c") 'my/check-treesit-grammars)
(define-key my-treesit-map (kbd "r") 'my/reinstall-grammar)
(define-key my-treesit-map (kbd "e") 'my/treesit-explore-current-buffer)
(define-key my-treesit-map (kbd "s") 'my/treesit-info)
(define-key my-treesit-map (kbd "m") 'my/toggle-treesit-inspect-mode)

;; =============================================================================
;; 10. 启动时自动配置
;; =============================================================================

(defun my/treesit-startup-setup ()
  "启动时的 tree-sitter 设置"
  (when (treesit-available-p)
    ;; 检查是否有缺失的语法文件
    (let ((missing-count 0))
      (dolist (lang-config treesit-language-source-alist)
        (unless (treesit-language-available-p (car lang-config))
          (setq missing-count (1+ missing-count))))
      
      (if (> missing-count 0)
          (progn
            (message "⚠️  发现 %d 个未安装的语法文件" missing-count)
            (message "💡 运行 C-c t i 安装所有语法文件"))
        (message "✅ 所有 tree-sitter 语法文件已就绪"))
      
      ;; 显示帮助信息
      (run-with-idle-timer 
       2 nil
       (lambda ()
         (message "🎯 Tree-sitter 快捷键: C-c t i(安装) c(检查) e(探索) s(状态)"))))))

;; 延迟执行启动设置
(run-with-idle-timer 1 nil 'my/treesit-startup-setup)

;; =============================================================================
;; 11. 可选的性能优化
;; =============================================================================

;; 调整 tree-sitter 相关的性能参数
(setq treesit-max-buffer-size (* 1024 1024 5)) ; 5MB 缓冲区限制

;; 在大文件中可能需要禁用某些功能以提升性能
(defun my/large-file-treesit-setup ()
  "大文件的 tree-sitter 优化设置"
  (when (and (treesit-parser-list)
             (> (buffer-size) 50000)) ; 50KB 以上的文件
    (setq-local treesit-font-lock-level 2) ; 降低语法高亮级别
    (message "大文件检测: 已降低 tree-sitter 语法高亮级别以提升性能")))

(add-hook 'find-file-hook 'my/large-file-treesit-setup)

;; =============================================================================
;; 完成信息
;; =============================================================================

(message "🚀 原生 Tree-sitter 配置已加载完成！")
(message "💡 首次使用请运行: C-c t i 安装语法文件")

(provide ‘TS-setting.el)
