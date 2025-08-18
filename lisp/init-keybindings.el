
;;; init-keybindings.el --- Keybindings configuration

;; 设置 Ctrl+, 快捷键打开配置文件
(defun open-config-file ()
  "快速打开 Emacs 配置文件"
  (interactive)
  (find-file user-init-file))
;; 绑定快捷键 Ctrl+c+o
(global-set-key (kbd "C-c C-o") 'open-config-file)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 在 normal 模式下绑定 Shift+H 和 Shift+L 只切换标签页中的 buffer
(evil-define-key 'normal 'global (kbd "H") 'centaur-tabs-backward)
(evil-define-key 'normal 'global (kbd "L") 'centaur-tabs-forward)

;; 在 insert 模式下绑定 C-h 和 C-l 左右移动
(evil-define-key 'insert 'global (kbd "C-h") 'backward-char)
(evil-define-key 'insert 'global (kbd "C-l") 'forward-char)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-act)
         ("C-c C-e" . embark-export)) ; 全局绑定
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package consult
  :ensure t)

(use-package avy
  :ensure t
  :after evil  ; 确保在 evil 加载后才加载 avy
  :config
  ;; Evil 模式绑定
  (evil-define-key '(normal visual) 'global (kbd "S") 'avy-goto-word-1)
  (evil-define-key '(normal visual) 'global (kbd "s") 'avy-goto-char))

;; 自定义 LSP treemacs symbols toggle 函数
(defun db/lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

(use-package consult
  :ensure t
  :config
  ;; 如果你使用 Evil，可以这样设置
 (with-eval-after-load 'evil
  (evil-set-leader 'normal (kbd "SPC"))
  
  ;; File operations
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
  
  ;; Search operations  
  (evil-define-key 'normal 'global (kbd "<leader>sb") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-lsp-file-symbols)
  (evil-define-key 'normal 'global (kbd "<leader>sS") 'consult-lsp-symbols)
  (evil-define-key 'normal 'global (kbd "<leader>sy") 'consult-yank-pop)
  (evil-define-key 'normal 'global (kbd "<leader>sd") 'consult-lsp-diagnostics)
  (evil-define-key 'normal 'global (kbd "<leader>sg") 'consult-ripgrep)))




;; 自定义 LSP treemacs symbols toggle 函数
(defun db/lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

(use-package general
  :ensure t
  :config
  (general-create-definer my/leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")
  
  (general-create-definer my/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC m"))

;; LazyVim 风格的按键绑定 - 优先使用 consult 和 lsp-mode
(with-eval-after-load 'evil
  (my/leader-keys
    ;; Files
    "fc" 'open-config-file
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer
    "fS" 'write-file
    "fb" 'consult-buffer
    "fp" 'consult-project-buffer
    "fP" 'project-switch-project
    "fd" 'project-find-dir
   
    ;;ui
    "us" 'my/modus

    ;; Buffers
    "sb" 'consult-buffer
    "bd" 'kill-current-buffer
    "bD" 'kill-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "br" 'revert-buffer
    "bs" 'scratch-buffer
    
    ;; Search
    "sn" 'consult-yasnippet
    "sb" 'consult-line
    "sB" 'consult-line-multi
    "sg" 'consult-ripgrep
    "sG" 'consult-git-grep
    "sy" 'consult-yank-pop
    "sd" 'consult-lsp-diagnostics
    "ss" 'consult-imenu
    "sS" 'imenu
    "sf" 'consult-find
    "sl" 'consult-locate
    "st" 'consult-todo
    "sT" 'consult-todo-project
    "sk" 'embark-bindings
    
    ;; Windows
    "w-" 'split-window-below
    "w|" 'split-window-right
    "wd" 'delete-window
    "wD" 'delete-other-windows
    "wh" 'windmove-left
    "wj" 'windmove-down
    "wk" 'windmove-up
    "wl" 'windmove-right
    "ww" 'other-window
    "w=" 'balance-windows
    "w+" 'enlarge-window
    "w_" 'shrink-window
    "w>" 'enlarge-window-horizontally
    "w<" 'shrink-window-horizontally
    
    ;; Quit/Session
    "qq" 'save-buffers-kill-terminal
    "qQ" 'save-buffers-kill-emacs
    
    ;; Toggle
    "tn" 'display-line-numbers-mode
    "tr" 'read-only-mode
    "tw" 'whitespace-mode
    "tW" 'whitespace-cleanup
    "th" 'hl-line-mode
    "ts" 'flyspell-mode
    
    ;; Code (LSP)
    "ca" 'lsp-execute-code-action
    "cr" 'lsp-rename
    "cf" 'lsp-format-buffer
    "cd" 'lsp-find-definition
    "cD" 'lsp-find-declaration
    "ci" 'lsp-find-implementation
    "ct" 'lsp-find-type-definition
    "cR" 'lsp-find-references
    "ch" 'lsp-describe-thing-at-point
    "cw" 'lsp-workspace-restart
    "cW" 'lsp-workspace-shutdown
    "cs" 'db/lsp-treemacs-symbols-toggle           ; 添加 LSP treemacs symbols
    
    ;; Git
    "gg" 'magit-status
    "gb" 'magit-blame
    "gf" 'magit-file-dispatch
    "gl" 'magit-log
    
    ;; Help
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key
    "hm" 'describe-mode
    "hp" 'describe-package
    
    ;; Open
    "ot" 'vterm
    "od" 'dired
    
   ;;compiler
    "rr" 'compiler-smart-run
    "ro" 'compiler-open
    "rR" 'compiler-redo
    "rt" 'compiler-toggle-results
    "rs" 'compiler-stop
    "rc" 'compiler-clean-output-now
    "cc" 'compiler-smart-run
    "cr" 'compiler-redo

    ;; LSP/Diagnostics
    "xx" 'consult-lsp-diagnostics
    "xn" 'flymake-goto-next-error
    "xp" 'flymake-goto-prev-error
    ))

;; 非 leader 的直接绑定
(with-eval-after-load 'evil
  ;; 文件操作
  (evil-define-key 'normal 'global (kbd "C-s") 'save-buffer)
  
  ;; 缓冲区导航
  (evil-define-key 'normal 'global (kbd "[b") 'previous-buffer)
  (evil-define-key 'normal 'global (kbd "]b") 'next-buffer)
  
  ;; 错误导航
  (evil-define-key 'normal 'global (kbd "[d") 'flymake-goto-prev-error)
  (evil-define-key 'normal 'global (kbd "]d") 'flymake-goto-next-error)
  
  ;; 快速注释
  (evil-define-key '(normal visual) 'global (kbd "gc") 'comment-or-uncomment-lines)
  (evil-define-key 'visual 'global (kbd "gc") 'comment-or-uncomment-region)
  )
;; 方案一：完全解绑 M-c（推荐）
(global-unset-key (kbd "M-c"))


(global-unset-key (kbd "M-c"))
;; 窗口调整
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)

;; which-key 的标签描述
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC f" "file"
    "SPC b" "buffer"  
    "SPC s" "search"
    "SPC w" "window"
    "SPC q" "quit/session"
    "SPC t" "toggle"
    "SPC c" "code"
    "SPC g" "git"
    "SPC h" "help"
    "SPC o" "open"
    "SPC x" "diagnostics"))

;; 需要的辅助函数
(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))



;; 默认加载 modus-operandi-tinted
(load-theme 'modus-operandi-tinted t)

(defun my/modus ()
  "swift the dark theme and light theme of modus"
  (interactive)
  (let ((current (car custom-enabled-themes)))
    (cond
     ((eq current 'modus-operandi-tinted)
      (disable-theme 'modus-operandi-tinted)
      (load-theme 'modus-vivendi-tinted t)
      (message "切换到深色主题"))
     ((eq current 'modus-vivendi-tinted)
      (disable-theme 'modus-vivendi-tinted)
      (load-theme 'modus-operandi-tinted t)
      (message "切换到浅色主题"))
     (t
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme 'modus-operandi-tinted t)
      (message "切换到 tinted 主题")))))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
