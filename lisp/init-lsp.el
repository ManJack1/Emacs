(use-package flymake
  :ensure t
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
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :config
  (setq sideline-flymake-display-mode 'line)  ; 'point, 'line, or 'all
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode))


(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)
	 (c++-ts-mode . lsp)
	 (c-ts-mode . lsp)  ; 修复：添加缺少的空格
         (c-mode . lsp)
	 (bash-ts-mode-hook . lsp)
	 (lua-ts-mode . lsp))
  :commands lsp
  :config
  ;;保存自动formmater
  (setq lsp-format-buffer-on-save t)
  ;; 使用 clangd 作为 C/C++ 语言服务器
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-prefer-flymake nil) ; 不用内置 flymake
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-idle-delay 0.5))  ; 修复：添加缺少的右括号

;;; init-lsp-ui.el --- Minimal lsp-ui setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal configuration for lsp-ui to enhance lsp-mode UI.
;;; Code:
(use-package lsp-ui
  :ensure t
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
  (evil-define-key 'normal lsp-ui-mode-map
    (kbd "gd") 'lsp-ui-peek-find-definitions
    (kbd "gr") 'lsp-ui-peek-find-references
    (kbd "K")  'lsp-ui-doc-glance
    (kbd "gi") 'lsp-ui-peek-find-implementation
    (kbd "gy") 'lsp-ui-peek-find-type-definition))

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




(use-package nerd-icons
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)        ; C/C++ 支持
   (python . t))) ; 保留之前的 Python 支持

(provide 'init-lsp)
;;; init-lsp.el ends here
