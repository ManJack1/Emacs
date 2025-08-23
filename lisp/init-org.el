;;; init-org.el --- Org mode configuration with straight.el
;; All Org mode settings go here

;;; Code:
;; Core Org mode configuration
(use-package org
  :straight (:host github :repo "emacs-mirror/emacs" :files ("lisp/org/*.el"))
  :hook ((org-mode . visual-line-mode)
         (org-mode . yas-minor-mode)
         (org-mode . laas-mode)
         ;; 只在 org-mode 中禁用左尖括号自动配对
         (org-mode . (lambda ()
                       (setq-local electric-pair-inhibit-predicate
                                   (lambda (c)
                                     (or (char-equal c ?\<)
                                         (and (boundp 'electric-pair-inhibit-predicate)
                                              (functionp electric-pair-inhibit-predicate)
                                              (funcall electric-pair-inhibit-predicate c))))))))
  :bind (:map org-mode-map
              ("C-c i i" . my/org-image-manager)
              ("C-c i f" . my/org-insert-image-file)
              ("C-c i u" . my/org-insert-image-url)
              ("C-c i s" . my/simple-screenshot)
              ("C-c C-m" . org-insert-matrix))
  :config
  ;; 启用 org-tempo (easy template)
  (require 'org-tempo)
  (require 'ob-tangle)
  
  ;; 基本外观设置
  (setq org-startup-indented t
        org-pretty-entities nil  ; 禁用 Unicode 替换
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(400)
        org-ellipsis " ▾"
        org-image-align 'center)
  
  ;; TODO 关键词
  (setq org-todo-keywords
        '((sequence " TODO" "⚡ DOING" "|" "✅ DONE" "❌ CANCELLED")))
  
  ;; 代码块设置
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  
  ;; 启用代码块执行功能
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (latex . t)))
  
  ;; 标题大小设置 - 针对 Modus theme 优化
  (add-hook 'org-mode-hook
            (lambda ()
              ;; 延迟执行，确保主题已加载
              (run-with-timer 0.1 nil
                              (lambda ()
                                (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
                                (set-face-attribute 'org-level-2 nil :height 1.3 :weight 'bold)
                                (set-face-attribute 'org-level-3 nil :height 1.2 :weight 'bold)
                                (set-face-attribute 'org-level-4 nil :height 1.1 :weight 'bold)
                                (set-face-attribute 'org-level-5 nil :height 1.05 :weight 'bold)))))
  
  ;; LaTeX预览设置
  (setq org-startup-with-latex-preview t
        org-preview-latex-default-process 'imagemagick
        org-format-latex-options
        `(:foreground default
          :background default
          :scale ,(cond ((eq system-type 'darwin) 1.2)      ; macOS
                        ((eq system-type 'gnu/linux) 2.0)   ; Linux
                        (t 1.5))                             ; 其他系统默认值
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 1.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-latex-preview-appearance-options
        '(:page-width 0.8
          :zoom 1.0
          :org-latex-preview-numbered t))
  
  ;; LaTeX导出设置
  (setq org-latex-listings t
        org-latex-src-block-backend 'minted
        org-latex-pdf-process 
        '("latexmk -xelatex -f -interaction=nonstopmode %f")
        org-latex-remove-logfiles t
        compilation-scroll-output t
        compilation-always-kill t
        compilation-ask-about-save nil)
  
  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  
  ;; 列表美化 - 替换 org-modern 的列表样式
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-+*]\\) "
       (0 (prog1 () 
            (let ((bullet (match-string-no-properties 1)))
              (compose-region (match-beginning 1) (match-end 1) 
                              (cond ((string= bullet "-") "•")
                                    ((string= bullet "+") "‣")
                                    ((string= bullet "*") "▸")
                                    (t bullet)))))))))
  
  ;; 复选框美化
  (font-lock-add-keywords 'org-mode
    '(("\\[X\\]" . '(:foreground "#4CAF50" :weight bold))
      ("\\[ \\]" . '(:foreground "#BDBDBD"))
      ("\\[-\\]" . '(:foreground "#FFA726" :weight bold))))
  
  ;; LaTeX 预览居中和对齐设置
  (add-hook 'org-latex-preview-open-functions
            (defun +org-latex-preview-uncenter (ov)
              (overlay-put ov 'justify (overlay-get ov 'before-string))
              (overlay-put ov 'before-string nil)))
  (add-hook 'org-latex-preview-close-functions
            (defun +org-latex-preview-recenter (ov)
              (overlay-put ov 'before-string (overlay-get ov 'justify))
              (overlay-put ov 'justify nil))))

;; Org superstar for beautiful headlines
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✤" "▶"))
  (org-superstar-item-bullet-alist '((?- . ?•) (?+ . ?‣) (?* . ?▸)))
  (org-superstar-leading-bullet ?\s)
  (org-superstar-special-todo-items t)
  :config
  ;; 隐藏leading stars
  (setq org-superstar-leading-fallback ?\s)
  (setq org-hide-leading-stars t))

;; Org download for image handling
(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "./images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-screenshot-method "screencapture -i %s")
  :config
  ;; 拖拽图片自动保存
  (org-download-enable))

;; LaTeX fragment toggle
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;; CDLaTeX 完整配置
(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (latex-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex))
  :config
  
  ;; =================================
  ;; 基本设置
  ;; =================================
  
  ;; 启用数学模式快速输入
  (setq cdlatex-math-symbol-alist
        '(
          ;; 希腊字母 (使用 ` 触发)
          (?a ("\\alpha" "\\aleph"))
          (?b ("\\beta" "\\beth"))
          (?g ("\\gamma" "\\Gamma"))
          (?d ("\\delta" "\\Delta"))
          (?e ("\\epsilon" "\\varepsilon"))
          (?z ("\\zeta"))
          (?h ("\\eta" "\\hbar"))
          (?q ("\\theta" "\\Theta" "\\vartheta"))
          (?i ("\\iota" "\\in"))
          (?k ("\\kappa"))
          (?l ("\\lambda" "\\Lambda"))
          (?m ("\\mu"))
          (?n ("\\nu" "\\nabla"))
          (?x ("\\xi" "\\Xi"))
          (?p ("\\pi" "\\Pi" "\\varpi"))
          (?r ("\\rho" "\\varrho"))
          (?s ("\\sigma" "\\Sigma" "\\varsigma"))
          (?t ("\\tau"))
          (?u ("\\upsilon" "\\Upsilon"))
          (?f ("\\phi" "\\Phi" "\\varphi"))
          (?c ("\\chi"))
          (?y ("\\psi" "\\Psi"))
          (?o ("\\omega" "\\Omega"))
          
          ;; 数学符号
          (?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow"))
          (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow"))
          (?= ("\\Leftrightarrow" "\\Longleftrightarrow"))
          (?. ("\\cdot" "\\circ"))
          (?* ("\\times" "\\star"))
          (?+ ("\\cup" "\\uplus"))
          (?- ("\\cap" "\\setminus"))
          (?8 ("\\infty"))
          (?0 ("\\emptyset" "\\varnothing"))
          (?6 ("\\partial"))
          (?& ("\\wedge" "\\bigwedge"))
          (?v ("\\vee" "\\bigvee"))
          (?^ ("\\uparrow" "\\Uparrow"))
          (?_ ("\\downarrow" "\\Downarrow"))
          ))
  
  ;; 数学修饰符 (使用 ' 触发)
  (setq cdlatex-math-modify-alist
        '(
          (?b "\\mathbf" nil t nil nil)      ; 粗体
          (?c "\\mathcal" nil t nil nil)     ; 花体
          (?r "\\mathrm" nil t nil nil)      ; 正体
          (?i "\\mathit" nil t nil nil)      ; 斜体
          (?s "\\mathsf" nil t nil nil)      ; 无衬线
          (?t "\\mathtt" nil t nil nil)      ; 等宽
          (?f "\\mathfrak" nil t nil nil)    ; 哥特体
          (?B "\\mathbb" nil t nil nil)      ; 黑板粗体
          (?. "\\dot" nil t nil nil)         ; 点
          (?: "\\ddot" nil t nil nil)        ; 双点
          (?~ "\\tilde" nil t nil nil)       ; 波浪线
          (?^ "\\hat" nil t nil nil)         ; 帽子
          (?v "\\vec" nil t nil nil)         ; 向量
          (?- "\\bar" nil t nil nil)         ; 横线
          (?_ "\\underline" nil t nil nil)   ; 下划线
          ))
  
  ;; 环境快速插入
  (setq cdlatex-env-alist
        '(
          ("equation" "\\begin{equation}\n?\n\\end{equation}\n" nil)
          ("align" "\\begin{align}\n?\n\\end{align}\n" nil)
          ("gather" "\\begin{gather}\n?\n\\end{gather}\n" nil)
          ("matrix" "\\begin{pmatrix}\n?\n\\end{pmatrix}" nil)
          ("cases" "\\begin{cases}\n?\n\\end{cases}" nil)
          ("split" "\\begin{split}\n?\n\\end{split}" nil)
          ))
  
  ;; 命令快速插入
  (setq cdlatex-command-alist
        '(
          ;; 分数和根式
          ("frac" "Insert fraction" "\\frac{?}{}" cdlatex-position-cursor nil nil nil)
          ("sqrt" "Insert square root" "\\sqrt{?}" cdlatex-position-cursor nil nil nil)
          ("nrt" "Insert nth root" "\\sqrt[?]{}" cdlatex-position-cursor nil nil nil)
          
          ;; 积分、求和、极限
          ("int" "Insert integral" "\\int_{?}^{} " cdlatex-position-cursor nil nil nil)
          ("oint" "Insert contour integral" "\\oint_{?}^{} " cdlatex-position-cursor nil nil nil)
          ("sum" "Insert sum" "\\sum_{?}^{}" cdlatex-position-cursor nil nil nil)
          ("prod" "Insert product" "\\prod_{?}^{}" cdlatex-position-cursor nil nil nil)
          ("lim" "Insert limit" "\\lim_{? \\to }" cdlatex-position-cursor nil nil nil)
          ("limsup" "Insert limsup" "\\limsup_{? \\to }" cdlatex-position-cursor nil nil nil)
          ("liminf" "Insert liminf" "\\liminf_{? \\to }" cdlatex-position-cursor nil nil nil)
          
          ;; 括号
          ("lr(" "Insert left-right parentheses" "\\left( ? \\right)" cdlatex-position-cursor nil nil nil)
          ("lr[" "Insert left-right brackets" "\\left[ ? \\right]" cdlatex-position-cursor nil nil nil)
          ("lr{" "Insert left-right braces" "\\left\\{ ? \\right\\}" cdlatex-position-cursor nil nil nil)
          ("lr|" "Insert left-right bars" "\\left| ? \\right|" cdlatex-position-cursor nil nil nil)
          ("lra" "Insert left-right angle brackets" "\\left\\langle ? \\right\\rangle" cdlatex-position-cursor nil nil nil)
          
          ;; 二项式和组合
          ("binom" "Insert binomial" "\\binom{?}{}" cdlatex-position-cursor nil nil nil)
          ("choose" "Insert choose" "{? \\choose }" cdlatex-position-cursor nil nil nil)
          
          ;; 文本
          ("text" "Insert text" "\\text{?}" cdlatex-position-cursor nil nil nil)
          ("mbox" "Insert mbox" "\\mbox{?}" cdlatex-position-cursor nil nil nil)
          ))
  
  ;; =================================
  ;; Org-mode 专用设置
  ;; =================================
  
  ;; 在 org-mode 中的特殊配置
  (add-hook 'org-mode-hook
            (lambda ()
              ;; 设置 CDLaTeX 只在 LaTeX 片段中工作
              (setq-local cdlatex-takeover-subsuperscript nil)
              (setq-local cdlatex-takeover-parenthesis nil)
              
              ;; 自定义快捷键
              (local-set-key (kbd "C-c {") 'cdlatex-environment)
              (local-set-key (kbd "C-c m") 'cdlatex-math-symbol)
              (local-set-key (kbd "C-c '") 'cdlatex-math-modify)
              
              ;; 在 LaTeX 环境中启用 TAB 补全
              (when (fboundp 'my/in-latex-context-p)
                (local-set-key (kbd "TAB")
                               (lambda ()
                                 (interactive)
                                 (if (my/in-latex-context-p)
                                     (cdlatex-tab)
                                   (org-cycle)))))))
  
  ;; =================================
  ;; 自定义函数增强
  ;; =================================
  
  ;; 智能上下标
  (defun my/cdlatex-smart-subscript ()
    "智能下标输入"
    (interactive)
    (if (my/in-latex-context-p)
        (cdlatex-sub-superscript ?_)
      (insert "_")))
  
  (defun my/cdlatex-smart-superscript ()
    "智能上标输入"
    (interactive)
    (if (my/in-latex-context-p)
        (cdlatex-sub-superscript ?^)
      (insert "^")))
  
  ;; 绑定智能上下标
  (global-set-key (kbd "C-c _") 'my/cdlatex-smart-subscript)
  (global-set-key (kbd "C-c ^") 'my/cdlatex-smart-superscript)
  
  ;; =================================
  ;; 与其他包的集成
  ;; =================================
  
  ;; 与 LaTeX-auto-activating-snippets 协同工作
  (when (featurep 'laas)
    ;; 设置优先级，让 LAAS 先处理简单情况
    (setq cdlatex-takeover-dollar nil)
    (setq cdlatex-paired-parens nil))
  
  ;; 与 YASnippet 协同工作
  (when (featurep 'yasnippet)
    ;; 在 CDLaTeX 失败时回退到 YASnippet
    (advice-add 'cdlatex-tab :after
                (lambda ()
                  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
                    (unless (cdlatex-tab)
                      (yas-expand)))))))

;;; Custom Functions

;; Stolen from the package ov
(defun ov-at (&optional point)
  "Get an overlay at POINT.
POINT defaults to the current `point'."
  (or point (setq point (point)))
  (car (overlays-at point)))

;; https://www.reddit.com/r/emacs/comments/169keg7/comment/jzierha/
(defun org-justify-fragment-overlay (beg end image &optional imagetype)
  "Only equations at the beginning and also end of a line are justified."
  (when (and (= beg (line-beginning-position)) 
             (= end (line-end-position)))
    (let* ((ov (ov-at))
           (disp (overlay-get ov 'display)))
      (overlay-put ov 'line-prefix `(space :align-to (- center (0.5 . ,disp)))))))

(advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)

;; LaTeX 环境检测
(defun my/in-latex-context-p ()
  "检查是否在 LaTeX 环境中"
  (or (org-inside-LaTeX-fragment-p)                    ; \( \) 或 \[ \] 内
      (org-in-block-p '("latex" "equation" "align"))   ; LaTeX 代码块内
      (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*\\\\[[(]"))))              ; 行首 LaTeX 命令

;; 普通 org 文本检测
(defun my/in-org-text-context-p ()
  "检查是否在普通 org 文本中"
  (not (my/in-latex-context-p)))

;; 代码块检测
(defun my/in-org-src-block-p ()
  "检查是否在源代码块中"
  (org-in-src-block-p))

;; 表格检测
(defun my/in-org-table-p ()
  "检查是否在 org 表格中"
  (org-at-table-p))

;; Complete image insertion manager
(defun my/org-image-manager ()
  "Complete image insertion manager for org-mode."
  (interactive)
  (let ((choice (completing-read 
                 "Insert image: "
                 '("File" "Screenshot" "Clipboard" "URL")
                 nil t)))
    (cond
     ((string= choice "File")
      (my/org-insert-image-file))
     ((string= choice "Screenshot")
      (if (fboundp 'org-download-screenshot)
          (org-download-screenshot)
        (my/simple-screenshot)))
     ((string= choice "Clipboard")
      (if (fboundp 'org-download-clipboard)
          (org-download-clipboard)
        (my/paste-image-from-clipboard)))
     ((string= choice "URL")
      (my/org-insert-image-url)))))

(defun my/org-insert-image-file ()
  "Insert image from file."
  (interactive)
  (let* ((file (read-file-name "Select image file: " nil nil t nil
                               (lambda (name) 
                                 (string-match-p "\\(\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\|\\.svg\\)\\'" name))))
         (image-dir (or org-download-image-dir "./images/"))
         (filename (file-name-nondirectory file))
         (local-path (expand-file-name filename image-dir)))
    
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    
    ;; Copy file if not already in images directory
    (unless (string= (expand-file-name (file-name-directory file)) 
                     (expand-file-name image-dir))
      (copy-file file local-path t))
    
    ;; Insert link
    (insert (format "[[file:%s]]\n" (file-relative-name local-path)))
    
    ;; Display image
    (when (fboundp 'org-display-inline-images)
      (org-display-inline-images))))

(defun my/org-insert-image-url ()
  "Insert image from URL."
  (interactive)
  (let* ((url (read-string "Image URL: "))
         (image-dir (or org-download-image-dir "./images/"))
         (extension (or (file-name-extension url) "png"))
         (filename (format "%s.%s" 
                          (format-time-string "%Y%m%d-%H%M%S")
                          extension))
         (local-path (expand-file-name filename image-dir)))
    
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    
    ;; Download image with error handling
    (condition-case err
        (progn
          (url-copy-file url local-path)
          ;; Insert link
          (insert (format "[[file:%s]]\n" (file-relative-name local-path)))
          ;; Display image
          (when (fboundp 'org-display-inline-images)
            (org-display-inline-images)))
      (error (message "Failed to download image: %s" (error-message-string err))))))

(defun my/simple-screenshot ()
  "Simple screenshot function with platform detection."
  (interactive)
  (let* ((image-dir (or org-download-image-dir "./images/"))
         (filename (format "screenshot_%s.png" 
                          (format-time-string "%Y%m%d-%H%M%S")))
         (local-path (expand-file-name filename image-dir))
         (cmd (cond
               ((eq system-type 'darwin) 
                (format "screencapture -i '%s'" local-path))
               ((eq system-type 'gnu/linux)
                (cond 
                 ((executable-find "gnome-screenshot")
                  (format "gnome-screenshot -a -f '%s'" local-path))
                 ((executable-find "scrot")
                  (format "scrot -s '%s'" local-path))
                 ((executable-find "import")
                  (format "import '%s'" local-path))
                 (t nil)))
               ((eq system-type 'windows-nt)
                ;; Windows screenshot (requires external tool)
                nil)
               (t nil))))
    
    (if cmd
        (progn
          (unless (file-exists-p image-dir)
            (make-directory image-dir t))
          
          (shell-command cmd)
          
          (when (file-exists-p local-path)
            (insert (format "[[file:%s]]\n" (file-relative-name local-path)))
            (when (fboundp 'org-display-inline-images)
              (org-display-inline-images))))
      (message "Screenshot not supported on this platform or missing tools"))))

(defun my/paste-image-from-clipboard ()
  "Paste image from clipboard."
  (interactive)
  (if (fboundp 'org-download-clipboard)
      (org-download-clipboard)
    (message "Clipboard image pasting requires org-download. Use org-download-clipboard manually.")))

;; Enhanced org-mode experience
(defun my/org-mode-setup ()
  "Setup function for org-mode."
  (interactive)
  ;; 自动换行
  (visual-line-mode 1)
  ;; 自动保存
  (auto-save-visited-mode 1)
  ;; 显示行号（可选）
  ;; (display-line-numbers-mode 1)
  )

(add-hook 'org-mode-hook 'my/org-mode-setup)

;; Matrix insertion function
(defun org-insert-matrix ()
  "Insert a LaTeX matrix template with placeholders"
  (interactive)
  (insert "$\\begin{pmatrix}\na & b \\\\\nc & d\n\\end{pmatrix}$")
  (search-backward "a")
  (set-mark (point))
  (forward-char 1))

;; Org agenda configuration (if needed)
(setq org-agenda-files '("~/org/"))  ; 根据需要调整路径

;; 全局禁用 Unicode 替换
(global-prettify-symbols-mode -1)
(add-hook 'org-mode-hook (lambda () (prettify-symbols-mode -1)))

;; Preview 增强
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble
                "\\PreviewEnvironment{tikzpicture}" t))

(provide 'init-org)
;;; init-org.el ends here
