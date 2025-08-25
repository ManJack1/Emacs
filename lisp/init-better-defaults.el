;;; init-better-defaults.el --- Better defaults

;; Enhance default behaviors here


;; 英文字体
(set-face-attribute 'default nil
                    :family "Maple Mono NF"
                    :height 150)

;; 中文字体 - 使用 Unicode 范围更精确
(when (member "LXGW WenKai Mono" (font-family-list))
  ;; 中日韩统一表意文字
  (set-fontset-font "fontset-default"
                    '(#x4e00 . #x9fff)
                    (font-spec :family "LXGW WenKai Mono"
                               :height 15))
  
  ;; 中日韩符号和标点
  (set-fontset-font "fontset-default"
                    '(#x3000 . #x303f)
                    (font-spec :family "LXGW WenKai Mono"
                               :height 15))
  
  ;; 全角字符
  (set-fontset-font "fontset-default"
                    '(#xff00 . #xffef)
                    (font-spec :family "LXGW WenKai Mono"
                               :height 15)))

(global-auto-revert-mode 1)
;; 完全关闭自动备份
(setq make-backup-files nil)
;; 关闭自动保存文件 (#filename#)
(setq auto-save-default nil)
(setq ring-bell-function nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; 全局开启视觉行模式（推荐）
(global-visual-line-mode 1)

;; 或者只在特定模式下开启
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)



;; Evil 基础配置
(use-package evil
  :straight t
  :init
  ;; Evil 配置必须在加载前设置
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; 使用 undo-fu 作为 undo 系统
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  
  ;; 让 C-g 在 insert 模式下也能回到 normal 模式
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  ;; 使用 vim 风格的视觉行移动
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; undo-fu 配置
(use-package undo-fu
  :straight t
  :after evil
  :config
  ;; 增加 undo 限制
  (setq undo-limit 67108864)        ; 64mb
  (setq undo-strong-limit 100663296) ; 96mb
  (setq undo-outer-limit 1006632960) ; 960mb
  
  ;; Evil 会自动绑定 u 和 C-r，但我们可以添加额外绑定
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

;; undo-fu-session - 持久化 undo 历史
(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" user-emacs-directory))
  (undo-fu-session-global-mode))

;; vundo - 可视化 undo 树
(use-package vundo
  :straight t
  :after evil
  :bind (:map evil-normal-state-map
         ("g u" . vundo))  ; 类似 Vim 的 :undotree
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-compact-display t)
  
  ;; vundo 中的 Evil 键绑定
  (define-key vundo-mode-map (kbd "h") 'vundo-backward)
  (define-key vundo-mode-map (kbd "l") 'vundo-forward)
  (define-key vundo-mode-map (kbd "j") 'vundo-next)
  (define-key vundo-mode-map (kbd "k") 'vundo-previous)
  (define-key vundo-mode-map (kbd "q") 'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") 'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") 'vundo-confirm))

;; 可选：evil-collection 用于更好的 Evil 集成
(straight-use-package 'evil-collection)
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))


(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
