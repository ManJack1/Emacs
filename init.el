;; -*- lexical-binding: t; -*-
;;; init.el --- Emacs 配置入口文件

;; ============================================================
;; 性能优化: 启动时增大 GC 阈值
;; ============================================================
(defvar my/gc-cons-threshold (* 16 1024 1024) "正常运行时的 GC 阈值")
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 启动时间测量
(defvar my/emacs-start-time (current-time)
  "Time when Emacs started.")

;; 引导 straight.el 包管理器
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 在加载 org-babel-load-file 之前先加载 straight 版本的 Org
(straight-use-package 'org)

;; 集成 use-package 与 straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; 加载配置
(load (expand-file-name "config.el" user-emacs-directory))

;; ;; ============================================================
;; ;; 启动完成后恢复 GC 阈值
;; ;; ============================================================
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold my/gc-cons-threshold
;;                   gc-cons-percentage 0.1)
;;             (message "Emacs 启动完成！用时: %s，GC: %d 次 (%.3fs)"
;;                      (emacs-init-time)
;;                      gcs-done
;;                      gc-elapsed)))

;; ;;; init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-agenda-files nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755" "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326" "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5" "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b" "3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "292a7482026054ebf039036f5f0a8cb670dea0c76bb8d34b6c9d74e19db8a9bc" "e36b78ef2b29a76c8487061af440de56e2b8481e6c9ef8cdc2a72cfd9d2475d2" "9113a2a0e6f13b8fe851c6c5a9b2a1a9608b9aae28b411c81211315b2e312007" "6352b7fab474438433ed2b1d82eff40379aca9cfa4495bf1d098a91706af485c" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93" "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd" "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" "c3076fdee603e9768817cfe8dbf6253d5b3cf3bf4602cb32fa2f1df62fe70b1c" "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:height 1.5 :weight bold))))
 '(org-level-2 ((t (:height 1.4))))
 '(org-level-3 ((t (:height 1.3))))
 '(org-level-4 ((t (:height 1.2))))
 '(org-level-5 ((t (:height 1.1))))
 '(org-level-6 ((t (:height 1.0)))))
