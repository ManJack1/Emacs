
;;; init-better-defaults.el --- Better defaults

;; Enhance default behaviors here

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

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
