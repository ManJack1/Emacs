;;; init.el --- Main configuration file

;; Add lisp subdirectory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))



(let ((start-time (current-time)))
  ;; 你的模块配置
  (use-package some-package
    :ensure t
    ;; ...
    )
  (message "Loaded some-package in %.2f seconds"
           (float-time (time-since start-time))))

;; Load modules
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-keybindings)
(require 'init-org)
(require 'init-lsp)
(require 'init-code)
(require 'compiler)
;; Load custom file
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
