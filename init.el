;;; init.el --- Main configuration file

;; Add lisp subdirectory to load-path
(add-to-list
 'load-path (expand-file-name "lisp" user-emacs-directory))

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
(require 'org-bars)
(require 'org-modern-indent)
(provide 'TS-setting)
;; Load custom file
(load custom-file 'noerror)

(setq org-agenda-files '("~/Workspace/org/")) ; 根据需要调整路径

(provide 'init)
;;; init.el ends here
