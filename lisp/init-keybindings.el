;;; init-keybindings.el --- Keybindings configuration with straight.el

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
  :straight t
  :bind (("C-." . embark-act)
         ("C-;" . embark-act)
         ("C-c C-e" . embark-export)) ; 全局绑定
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package consult
  :straight t)

(use-package avy
  :straight t
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

(use-package general
  :straight t
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
    ;;agenda
    "aa" 'org-agenda-file-to-front
    "A" 'org-agenda
    ;; Files
    "fc" 'my-find-config-files
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer
    "fS" 'write-file
    "fb" 'consult-buffer
    "fp" 'project-find-file
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
    "sd" 'consult-flymake
    "ss" 'consult-imenu
    "sS" 'imenu
    "sf" 'consult-find
    "sl" 'consult-locate
    "st" 'consult-todo
    "sT" 'consult-todo-project
    "sk" 'embark-bindings
    "sm" 'consult-bookmark
    "bm" 'bookmark-set

    
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
    "cl" 'describe-mode
    "ct" 'lsp-find-type-definition
    "cR" 'lsp-find-references
    "ch" 'lsp-describe-thing-at-point
    "cw" 'lsp-workspace-restart
    "cW" 'lsp-workspace-shutdown
    "cs" 'db/lsp-treemacs-symbols-toggle           ; 添加 LSP treemacs symbols

    ;;dirvish
    "e" 'my-dirvish-toggle 

    
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
    "mm" 'compiler-smart-run
    "mt" 'compiler-open
    "mr" 'compiler-redo
    "mt" 'compiler-toggle-results
    "ms" 'compiler-stop
    "mc" 'compiler-clean-output-now

    ;; LSP/Diagnostics
    "xx" 'consult-lsp-diagnostics
    "xn" 'flymake-goto-next-error
    "xp" 'flymake-goto-prev-error

    ;; org/dowload
    "ip" 'org-download-clipboard
    "id" 'org-download-delete

    ;;org/edit-code
    "oc" 'org-edit-src-code
    "oe" 'org-edit-src-exit
    "or" 'org-babel-execute-src-block
    "od" 'org-ctrl-c-ctrl-c
    "ot" 'org-todo
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
(load-theme 'doom-one t)

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

;; Cross-platform ls fix for macOS/BSD and Linux
(cond
 ;; macOS/BSD - disable --dired by default, use gls if available
 ((or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls")
    (setq dired-use-ls-dired t)))
 ;; Linux - GNU ls should work fine
 ((eq system-type 'gnu/linux)
  (setq dired-use-ls-dired t)))

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :config
  ;; Basic settings
  (setq dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Downloads/" "Downloads")
          ("D" "~/Desktop/" "Desktop")
          ("p" "~/Projects/" "Projects")
          ("e" "~/.emacs.d/" "Emacs config")
          ("t" "/tmp/" "Temporary files")))
  
  ;; Enable useful attributes - order matters
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  
  ;; Enable mode line info
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  
  ;; Enable header line
  (setq dirvish-header-line-format
        '(:left (path) :right (free-space)))
  
  ;; Standard preview dispatchers (from official docs)
  (setq dirvish-preview-dispatchers '(image gif video audio epub pdf archive))
  
  ;; Auto preview settings
  (setq dirvish-preview-policy '(("*" . "*")))  ; Preview all files
  (setq dirvish-idle-time 0.1)
  
  ;; Use fd for faster file searching if available
  (when (executable-find "fd")
    (setq dirvish-fd-switches "-H"))
  
  ;; Fullscreen settings
  (setq dirvish-fullscreen-props '((window-width . 0.8)
                                   (window-height . 0.8)))
  
  ;; Enable emerge and side preview
  (setq dirvish-emerge-groups '("*.png" "*.jpg" "*.jpeg" "*.gif" "*.svg"
                               "*.mp4" "*.mkv" "*.avi" "*.mov"
                               "*.pdf" "*.doc" "*.docx" "*.xls" "*.xlsx"
                               "*.zip" "*.tar" "*.gz" "*.7z"))
  
  ;; Optional: Enable side window
  (setq dirvish-side-width 40)
  
  ;; Key bindings for dirvish
  (define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "M-f") 'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") 'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") 'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m") 'dirvish-mark-actions-menu)
  (define-key dirvish-mode-map (kbd "M-t") 'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") 'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") 'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") 'dirvish-fd-jump)
  (define-key dirvish-mode-map (kbd "v") 'dirvish-toggle-preview)
  
  ;; Evil mode key bindings
  (with-eval-after-load 'evil
    (evil-define-key 'normal dirvish-mode-map
      (kbd "RET") 'dired-find-file
      (kbd "<return>") 'dired-find-file
      "h" 'dired-up-directory
      "l" 'dired-find-file
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "o" 'dired-find-file-other-window
      "q" 'dirvish-quit
      "gg" 'beginning-of-buffer
      "G" 'end-of-buffer
      "^" 'dired-up-directory
      "?" 'dirvish-dispatch
      "A" 'dirvish-setup-menu              ; Keep original A for dirvish setup
      "f" 'dirvish-file-info-menu
      "s" 'dirvish-quicksort
      "R" 'dirvish-history-jump             ; Move history to R
      "v" 'dirvish-toggle-preview
      "gr" 'revert-buffer
      
      ;; NeoTree-like file operations
      "m" 'dired-mark
      "u" 'dired-unmark
      "U" 'dired-unmark-all-marks
      "t" 'dired-toggle-marks
      
      ;; File/directory operations (NeoTree style)
      "a" 'my-dirvish-create-file        ; add file/directory
      "d" 'my-dirvish-delete             ; delete
      "r" 'my-dirvish-rename             ; rename
      "y" 'my-dirvish-copy               ; yank/copy
      "x" 'my-dirvish-cut                ; cut
      "p" 'my-dirvish-paste              ; paste
      
      ;; Alternative bindings
      "+" 'my-dirvish-create-directory   ; Still keep + for directory creation
      "TAB" 'dirvish-subtree-toggle)))

;; Helper functions
(defun my-dirvish-open-home ()
  "Open home directory with dirvish."
  (interactive)
  (dirvish "~/"))

(defun my-dirvish-open-current ()
  "Open current directory with dirvish."
  (interactive)
  (dirvish default-directory))

(defun my-dirvish-side-toggle ()
  "Toggle dirvish side window."
  (interactive)
  (if (dirvish-side--session-visible-p)
      (dirvish-side-close)
    (dirvish-side)))

(defun my-dirvish-fullscreen ()
  "Open dirvish in fullscreen."
  (interactive)
  (dirvish-dwim))

;; NeoTree-like file operations
(defvar my-dirvish-clipboard nil
  "Clipboard for cut/copy operations.")

(defvar my-dirvish-clipboard-action nil
  "Action type: 'copy or 'cut.")

(defun my-dirvish-create-file ()
  "Create a new file or directory (NeoTree style).
If path ends with '/', create a directory; otherwise create a file."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (input (read-string "Create (end with / for directory): " current-dir))
         (path (expand-file-name input)))
    (when (and input (not (string-empty-p input)))
      (if (file-exists-p path)
          (message "Already exists: %s" (file-name-nondirectory path))
        (if (string-suffix-p "/" input)
            ;; Create directory
            (progn
              (make-directory path t)
              (revert-buffer)
              (dired-goto-file path)
              (message "Created directory: %s" (file-name-nondirectory (directory-file-name path))))
          ;; Create file
          (progn
            ;; Create parent directories if needed
            (let ((parent-dir (file-name-directory path)))
              (when (not (file-exists-p parent-dir))
                (make-directory parent-dir t)))
            ;; Create empty file
            (with-temp-buffer
              (write-file path))
            (revert-buffer)
            (dired-goto-file path)
            (message "Created file: %s" (file-name-nondirectory path))))))))

(defun my-dirvish-create-directory ()
  "Create a new directory (NeoTree style)."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (dirname (read-string "Create directory: " current-dir)))
    (when (and dirname (not (string-empty-p dirname)))
      ;; Auto-add trailing slash if not present
      (unless (string-suffix-p "/" dirname)
        (setq dirname (concat dirname "/")))
      (let ((path (expand-file-name dirname)))
        (if (file-exists-p path)
            (message "Directory already exists: %s" (file-name-nondirectory (directory-file-name path)))
          (make-directory path t)
          (revert-buffer)
          (dired-goto-file path)
          (message "Created directory: %s" (file-name-nondirectory (directory-file-name path))))))))

(defun my-dirvish-rename ()
  "Rename file or directory (NeoTree style)."
  (interactive)
  (let* ((file (dired-get-filename))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (new-name (read-string "Rename to: " name)))
    (when (and new-name (not (string-empty-p new-name)) (not (string= name new-name)))
      (let ((new-file (expand-file-name new-name dir)))
        (if (file-exists-p new-file)
            (message "File already exists: %s" new-name)
          (rename-file file new-file)
          (revert-buffer)
          (dired-goto-file new-file)
          (message "Renamed %s to %s" name new-name))))))

(defun my-dirvish-delete ()
  "Delete file or directory with confirmation (NeoTree style)."
  (interactive)
  (let ((file (dired-get-filename)))
    (when (yes-or-no-p (format "Delete %s? " (file-name-nondirectory file)))
      (if (file-directory-p file)
          (delete-directory file t)
        (delete-file file))
      (revert-buffer)
      (message "Deleted: %s" (file-name-nondirectory file)))))

(defun my-dirvish-copy ()
  "Copy file to clipboard (NeoTree style)."
  (interactive)
  (let ((file (dired-get-filename)))
    (setq my-dirvish-clipboard file
          my-dirvish-clipboard-action 'copy)
    (message "Copied: %s" (file-name-nondirectory file))))

(defun my-dirvish-cut ()
  "Cut file to clipboard (NeoTree style)."
  (interactive)
  (let ((file (dired-get-filename)))
    (setq my-dirvish-clipboard file
          my-dirvish-clipboard-action 'cut)
    (message "Cut: %s" (file-name-nondirectory file))))

(defun my-dirvish-paste ()
  "Paste file from clipboard (NeoTree style)."
  (interactive)
  (when my-dirvish-clipboard
    (let* ((source my-dirvish-clipboard)
           (target-dir (dired-current-directory))
           (target (expand-file-name (file-name-nondirectory source) target-dir)))
      (cond
       ((eq my-dirvish-clipboard-action 'copy)
        (if (file-directory-p source)
            (copy-directory source target)
          (copy-file source target))
        (message "Copied %s to %s" (file-name-nondirectory source) target-dir))
       ((eq my-dirvish-clipboard-action 'cut)
        (rename-file source target)
        (setq my-dirvish-clipboard nil
              my-dirvish-clipboard-action nil)
        (message "Moved %s to %s" (file-name-nondirectory source) target-dir)))
      (revert-buffer)
      (dired-goto-file target))))

(defun my-dirvish-toggle ()
  "Toggle dirvish layout or quit if already in dirvish."
  (interactive)
  (let ((current-session (dirvish-curr)))
    (if current-session
        ;; If there's an active dirvish session, quit it
        (progn
          (dirvish-quit)
          (message "Dirvish closed"))
      ;; Otherwise open dirvish
      (progn
        (dirvish default-directory)
        (message "Dirvish opened")))))

;; Find Emacs configuration files
(defun my-find-config-files ()
  "Find and open Emacs configuration files from init.el and lisp/ directory."
  (interactive)
  (let* ((config-dir user-emacs-directory)
         (lisp-dir (expand-file-name "lisp" config-dir))
         (init-file user-init-file)
         (files (append
                ;; Add init.el if it exists
                (when (file-exists-p init-file)
                  (list init-file))
                ;; Add all .el files from lisp/ directory
                (when (file-directory-p lisp-dir)
                  (directory-files-recursively lisp-dir "\\.el$"))
                ;; Add other common config files in emacs.d
                (cl-remove-if-not #'file-exists-p
                                  (mapcar (lambda (f) (expand-file-name f config-dir))
                                          '("early-init.el" "custom.el" "config.el")))))
         ;; Create relative paths for display
         (choices (mapcar (lambda (file)
                           (file-relative-name file config-dir))
                         files)))
    (if choices
        (let* ((selected (completing-read "Open config file: " choices))
               (full-path (expand-file-name selected config-dir)))
          (find-file full-path))
      (message "No configuration files found"))))

(defun my-edit-init ()
  "Open init.el"
  (interactive)
  (find-file user-init-file))

(defun my-browse-emacs-config ()
  "Browse .emacs.d directory"
  (interactive)
  (dired user-emacs-directory))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
