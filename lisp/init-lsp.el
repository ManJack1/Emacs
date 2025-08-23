;;; init-lsp.el --- LSP configuration with straight.el -*- lexical-binding: t; -*-

(use-package flymake
  :straight nil  ; 使用 Emacs 内置版本
  :hook (prog-mode . flymake-mode)
  :config
  ;; 基本设置
  (setq flymake-no-changes-timeout 0.5
        flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t)
  
  ;; 自定义错误显示函数
  (defun my/flymake-show-error-at-point ()
    "Show flymake error at point in minibuffer."
    (interactive)
    (when flymake-mode
      (let ((diagnostics (flymake-diagnostics (point))))
        (if diagnostics
            (message "%s" (flymake-diagnostic-text (car diagnostics)))
          (message "No flymake diagnostics at point")))))
  
  ;; 自动显示错误
  (defvar my/flymake-timer nil)
  (defun my/flymake-show-error-delayed ()
    "Show error after a delay."
    (when my/flymake-timer
      (cancel-timer my/flymake-timer))
    (setq my/flymake-timer
          (run-with-timer 0.5 nil 'my/flymake-show-error-at-point)))
  
  ;; 移动光标时显示错误
  (add-hook 'post-command-hook 
            (lambda ()
              (when flymake-mode
                (my/flymake-show-error-delayed))))
  
  :bind (("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)
         ("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f d" . my/flymake-show-error-at-point)
         ("C-c f s" . flymake-start)))

;; 错误高亮样式
(custom-set-faces
 '(flymake-error ((t (:underline (:color "red" :style wave)))))
 '(flymake-warning ((t (:underline (:color "orange" :style wave)))))
 '(flymake-note ((t (:underline (:color "blue" :style wave))))))

(use-package sideline
  :hook (flymake-mode . sideline-mode)
  :config
  (setq sideline-flymake-display-mode 'line)  ; 'point, 'line, or 'all
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode))

(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c++-ts-mode . lsp)
         (c-ts-mode . lsp)
         (c-mode . lsp)
         (bash-ts-mode . lsp)  ; 修复：移除多余的 -hook
         (lua-ts-mode . lsp))
  :commands lsp
  :config
  ;; 保存自动 formatter
  (setq lsp-format-buffer-on-save t)
  ;; 使用 clangd 作为 C/C++ 语言服务器
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-prefer-flymake nil) ; 不用内置 flymake
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-idle-delay 0.5))

;;; LSP UI 配置
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; 悬浮文档
  (setq lsp-ui-doc-enable t)          ; 开启悬浮文档
  (setq lsp-ui-doc-delay 0.2)         ; 显示延迟
  (setq lsp-ui-doc-position 'at-point); 在光标处显示
  (setq lsp-ui-doc-use-childframe t)  ; 使用 child frame
  ;; 边栏提示
  (setq lsp-ui-sideline-enable t)          ; 开启边栏
  (setq lsp-ui-sideline-show-hover t)      ; 悬浮提示
  (setq lsp-ui-sideline-show-diagnostics nil); 显示错误信息
  (setq lsp-ui-sideline-show-code-actions nil); 显示可用操作
  ;; 绑定常用功能
  (setq lsp-ui-doc-show-with-cursor nil)
  ;; 常用按键绑定
  (with-eval-after-load 'evil
    (evil-define-key 'normal lsp-ui-mode-map
      (kbd "gd") 'lsp-ui-peek-find-definitions
      (kbd "gr") 'lsp-ui-peek-find-references
      (kbd "K")  'lsp-ui-doc-glance
      (kbd "gi") 'lsp-ui-peek-find-implementation
      (kbd "gy") 'lsp-ui-peek-find-type-definition)))

;; lsp-treemacs
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-symbols lsp-treemacs-errors-list))

;; Evil 模式集成
(with-eval-after-load 'evil
  (defun my/evil-write-with-format ()
    "Evil write with clang-format."
    (interactive)
    (when (derived-mode-p 'c++-mode 'c-mode)
      (if (fboundp 'lsp-format-buffer)
          (lsp-format-buffer)
        (my/clang-format-buffer-manual)))
    (evil-write nil nil nil nil nil))
  
  (evil-ex-define-cmd "w[rite]" 'my/evil-write-with-format)
  (evil-ex-define-cmd "W[rite]" 'my/evil-write-with-format))

;; nerd-icons（如果其他地方没有配置的话）
(use-package nerd-icons)

;; 辅助函数（如果需要的话）
(defun my/clang-format-buffer-manual ()
  "Manual clang-format buffer if available."
  (when (executable-find "clang-format")
    (shell-command-on-region (point-min) (point-max) 
                             "clang-format" 
                             (current-buffer) t)))

(provide 'init-lsp)
;;; init-lsp.el ends here
