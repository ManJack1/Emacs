;;; init.el --- Main configuration file
;; 修复 macOS 上原生编译库路径问题
;; 设置 LIBRARY_PATH 环境变量，让 libgccjit 能找到必要的 GCC 库文件如 libemutls_w

;; 适用于使用 Homebrew 的 Apple Silicon (M1/M2) Mac：
(when (and (eq system-type 'darwin) 
           (string-match "arm64\\|aarch64" system-configuration))
  (setenv "LIBRARY_PATH" 
          (string-join
           '("/opt/homebrew/Cellar/gcc/15.1.0/lib/gcc/current/gcc/aarch64-apple-darwin24/15"
             "/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin24/15" 
             "/opt/homebrew/lib/gcc/current"
             "/opt/homebrew/lib/gcc/15"
             "/opt/homebrew/lib")
           ":")))
;; Add lisp subdirectory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; Load modules
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-keybindings)
(require 'init-org)
(require 'init-lsp)
(require 'init-code)
(require 'compiler)
(require 'init-ultisnippet)
(require 'laas-org-math-detection)
;; Load custom file
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here



