
;;; init-org.el --- Org mode configuration

;; All Org mode settings go here

(use-package org
  :ensure t
  :config
  ;; 启用 org-tempo (easy template)
  (require 'org-tempo)
  
  :hook
  ;; 只在 org-mode 中禁用左尖括号自动配对
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                           (lambda (c)
                             (or (char-equal c ?\<)
                                 (and (boundp 'electric-pair-inhibit-predicate)
                                      (functionp electric-pair-inhibit-predicate)
                                      (funcall electric-pair-inhibit-predicate c))))))))


(use-package org
  :ensure t
  :config
  ;; org-install 的功能在现代 Emacs 中已经被自动处理
  ;; 不需要单独 require
  
  ;; ob-tangle 是 org-babel 的一部分，直接可用
  ;; 如果需要启用代码块执行功能：
  (require 'ob-tangle)  ; 用于代码块导出
  
  ;; 启用其他 org-babel 语言支持（可选）
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     ;; 添加其他需要的语言
     )))

;; 简约而美观的Org配置
(use-package org
  :ensure t
  :hook ((org-mode . visual-line-mode)
	(org-mode . yas-minor-mode))
  :config
  ;; 基本设置
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(400))
  
  ;; 美化符号
  (setq org-ellipsis " ▾")
  
  ;; TODO 关键词
  (setq org-todo-keywords
        '((sequence " TODO" "⚡ DOING" "|" "✅ DONE" "❌ CANCELLED")))
  
  ;; 标题大小
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face)))
  
  ;; 代码块美化
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  
  ;; 列表美化
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; 复选框美化
  (font-lock-add-keywords 'org-mode
    '(("\\[X\\]" . '(:foreground "#4CAF50" :weight bold))
      ("\\[ \\]" . '(:foreground "#BDBDBD")))))

;; 添加org-superstar用于更好的标题符号
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✤")))


;; 显示详细编译过程的配置
;; 确保 tcolorbox 包可用
(setq org-latex-listings t
      org-latex-src-block-backend 'listings)

;; 使用 XeLaTeX
(setq org-latex-pdf-process 
      '("latexmk -xelatex -f -interaction=nonstopmode %f"))

(add-to-list 'org-latex-packages-alist '("" "minted" nil))
(setq org-latex-src-block-backend 'minted)


;; 显示编译缓冲区
(setq org-latex-remove-logfiles t)           ; 保留日志文件
(setq compilation-scroll-output t)             ; 自动滚动输出
(setq compilation-always-kill t)               ; 总是结束之前的编译
(setq compilation-ask-about-save nil)          ; 编译前自动保存


(provide 'init-org)
;;; init-org.el ends here
