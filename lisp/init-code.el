
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF4500")
          ("NOTE"   . "#1E90FF")
          ("BUG"    . "#FF1493"))))


 (use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))


(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  
  (defun my/tabout ()
    "Jump out of brackets, quotes, etc., with org-mode support."
    (interactive)
    (cond
     ;; 在 Org mode 中优先使用 org-cycle
     ((and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (org-cycle))
     
     ;; 在 Org mode 的其他位置也检查是否需要 org-cycle
     ((and (derived-mode-p 'org-mode)
           (or (org-at-item-p)
               (org-at-table-p)
               (org-at-block-p)))
      (org-cycle))
     
     ;; yasnippet 展开
     ((and (fboundp 'yas-expand) (yas-expand)))
     
     ;; yasnippet 下一个字段
     ((and (fboundp 'yas-active-snippets) (yas-active-snippets))
      (yas-next-field))
     
     ;; 增加对尖括号的识别
     ((and (not (eolp))
           (memq (char-after) '(?\) ?\] ?\} ?\> ?\" ?\' ?\`)))
      (forward-char 1))
     
     ;; 查找所有闭合字符，包括尖括号
     ((re-search-forward "[])}>\"`']" (line-end-position) t))
     
     ;; 在 Org mode 中，如果没有其他匹配，回退到 org-cycle
     ((derived-mode-p 'org-mode)
      (org-cycle))
     
     ;; 其他模式使用默认的 tab 行为
     (t (indent-for-tab-command))))
  
  (defun my/tab-back ()
    "Jump backward out of brackets, quotes, etc., with org-mode support."
    (interactive)
    (cond
     ;; 在 Org mode 中使用 org-shifttab
     ((derived-mode-p 'org-mode)
      (org-shifttab))
     
     ;; yasnippet 上一个字段
     ((and (fboundp 'yas-active-snippets) (yas-active-snippets))
      (yas-prev-field))
     
     ;; 使用 smartparens 向上跳出当前表达式
     ((and (bound-and-true-p smartparens-mode)
           (condition-case nil
               (save-excursion (sp-backward-up-sexp) t)
             (error nil)))
      (sp-backward-up-sexp))
     
     ;; 手动查找匹配的开括号/引号并跳到它前面
     ((let ((current-syntax (syntax-ppss)))
        (cond
         ;; 如果在字符串内（单引号、双引号、反引号）
         ((nth 3 current-syntax)
          (goto-char (nth 8 current-syntax))
          t)
         ;; 如果在括号内（包括尖括号）
         ((nth 1 current-syntax)
          (goto-char (nth 1 current-syntax))
          t)
         ;; 手动查找最近的开符号
         ((save-excursion
            (re-search-backward "[([{<\"`']" (line-beginning-position) t))
          (re-search-backward "[([{<\"`']" (line-beginning-position) t)
          t)
         ;; 没找到任何可跳出的符号
         (t nil))))
     
     ;; 如果都没有匹配，什么都不做
     (t (message "Nothing to jump out of"))))
  
  ;; 在 Evil 插入模式下绑定（排除 org-mode）
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "<tab>") 'my/tabout)
    (define-key evil-insert-state-map (kbd "<backtab>") 'my/tab-back)
    
    ;; 为 Org mode 创建特殊的键绑定
    (evil-define-key 'insert org-mode-map (kbd "<tab>") 'my/tabout)
    (evil-define-key 'insert org-mode-map (kbd "<backtab>") 'my/tab-back)
    (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
    (evil-define-key 'normal org-mode-map (kbd "<backtab>") 'org-shifttab))
  
  ;; 在 smartparens-mode 下绑定，但不覆盖 org-mode 的键绑定
  (define-key smartparens-mode-map (kbd "<tab>") 'my/tabout)
  (define-key smartparens-mode-map (kbd "<backtab>") 'my/tab-back)
  
  ;; 确保 org-mode 中的 Tab 键优先级更高
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "<tab>") 'my/tabout)
    (define-key org-mode-map (kbd "<backtab>") 'my/tab-back)))

(use-package which-key
 :ensure t
 :init (which-key-mode)
 :diminish which-key-mode
 :config
 (setq which-key-idle-delay 0))




(use-package copilot
  :ensure t
  :bind (("M-<tab>" . copilot-accept-completion)
         ("M-TAB" . copilot-accept-completion))
  :config
  ;; 禁用特定的警告
  (setq warning-suppress-log-types '((copilot)))
  ;; 或者只禁用这个特定警告
  (setq warning-suppress-types '((copilot copilot--infer-indentation-offset))))


(provide 'init-code)
