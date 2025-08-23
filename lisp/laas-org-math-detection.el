;; 改进的 LAAS Org 数学环境检测函数
;; 支持 \\\\ 换行符，修复所有语法错误

;; 兼容性函数定义
(unless (fboundp 'oddp)
  (defun oddp (num)
    "Return t if NUM is odd, nil otherwise."
    (= (% num 2) 1)))

(defvar laas-org-math-environments
  '("equation" "equation*" "align" "align*" "alignat" "alignat*" 
    "flalign" "flalign*" "gather" "gather*" "multline" "multline*"
    "eqnarray" "eqnarray*" "split" "subequations" "cases"
    "matrix" "pmatrix" "bmatrix" "vmatrix" "Vmatrix" "smallmatrix"
    "array" "tabular" "aligned" "gathered" "alignedat"
    "math" "displaymath")
  "LAAS 支持的数学环境列表")

(defvar laas-org-math-macros
  '("\\ensuremath" "\\boxed")
  "LAAS 支持的数学宏列表")

;; === 核心检测函数 ===

(defun laas-org--get-search-bound ()
  "获取搜索边界，支持跨越换行的数学环境"
  (save-excursion
    (let ((paragraph-count 0)
          (max-paragraphs 3))
      (while (and (< paragraph-count max-paragraphs)
                  (re-search-backward "[\n\r][ \t]*[\n\r]" nil t))
        (setq paragraph-count (1+ paragraph-count)))
      (if (> paragraph-count 0)
          (match-beginning 0)
        (point-min)))))

(defun laas-org--match-environment (bound)
  "匹配 LaTeX 环境"
  (catch 'found
    (save-excursion
      (let (end-list env)
        (while (re-search-backward 
                "\\\\\\(begin\\|end\\)[ \t]*{\\([^}]+\\)}" bound t)
          (setq env (match-string-no-properties 2))
          (cond 
           ((string= (match-string-no-properties 1) "end")
            (push env end-list))
           ((and end-list (string= env (car end-list)))
            (pop end-list))
           ((member env laas-org-math-environments)
            (throw 'found (cons env (point))))))
        nil))))

(defun laas-org--match-macro (bound)
  "匹配数学宏"
  (catch 'found
    (save-excursion
      (let ((orig-point (point))
            pos cmd)
        (while (and (> (point) bound)
                    (re-search-backward "\\\\[a-zA-Z*]+" bound t))
          (setq pos (point)
                cmd (match-string-no-properties 0))
          (when (member cmd laas-org-math-macros)
            (goto-char (match-end 0))
            (skip-chars-forward " \t\n")
            (when (and (looking-at "{")
                       (condition-case nil
                           (progn
                             (forward-sexp 1)
                             (>= (point) orig-point))
                         (error nil)))
              (throw 'found (cons cmd pos))))
          (goto-char pos))
        nil))))

(defun laas-org--match-switch (bound)
  "匹配数学开关"
  (catch 'found
    (save-excursion
      (let ((switches-regexp "\\\\\\([][()]\\)"))
        (while (re-search-backward switches-regexp bound t)
          (let ((switch (match-string-no-properties 1)))
            (when (member switch '("[" "("))
              (throw 'found (cons (concat "\\" switch) (match-beginning 0))))))
        nil))))

(defun laas-org--check-togglers (bound pos match-pos)
  "检查切换符，支持换行符"
  (save-excursion
    (goto-char match-pos)
    (let ((dollar-count 0)
          (double-dollar-count 0))
      
      (while (< (point) pos)
        (cond
         ((and (looking-at "\\$\\$")
               (not (looking-back "\\\\\\\\[ \t]*" 10)))
          (setq double-dollar-count (1+ double-dollar-count))
          (forward-char 2))
         ((and (looking-at "\\$")
               (not (looking-at "\\$\\$"))
               (not (looking-back "\\\\\\$" 2))
               (not (looking-back "\\\\\\\\[ \t]*" 10)))
          (setq dollar-count (1+ dollar-count))
          (forward-char 1))
         ((looking-at "\\\\\\\\")
          (forward-char 2)
          (skip-chars-forward " \t\n"))
         ((looking-at "\\\\.")
          (forward-char 2))
         (t
          (forward-char 1))))
      
      (or (oddp double-dollar-count)
          (oddp dollar-count)))))

(defun laas-org--is-math-command (cmd)
  "判断命令是否为数学命令"
  (when cmd
    (or (member cmd laas-org-math-environments)
        (member cmd laas-org-math-macros)
        (member cmd '("\\[" "\\(" "$$")))))

(defun laas-org--in-org-latex-block-p ()
  "检测是否在 Org LaTeX 代码块中"
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-at-point)))
      (and (eq (org-element-type element) 'src-block)
           (string= (org-element-property :language element) "latex")))))

(defun laas-org--check-math-with-linebreaks ()
  "检查包含换行符的数学环境"
  (save-excursion
    (let ((current-pos (point))
          (line-start (line-beginning-position))
          (line-end (line-end-position)))
      
      (when (save-excursion
              (goto-char line-start)
              (re-search-forward "\\\\\\\\[ \t]*$" line-end t))
        
        (let ((math-start nil)
              (search-lines 10))
          (dotimes (i search-lines)
            (forward-line -1)
            (beginning-of-line)
            (when (or (looking-at "[ \t]*\\\\begin{\\(align\\|gather\\|multline\\)")
                      (looking-at "[ \t]*\\\\\\[")
                      (looking-at "[ \t]*\\$\\$"))
              (setq math-start (point))
              (cl-return)))
          
          (when math-start
            (goto-char math-start)
            (let ((env-name (cond
                             ((looking-at "[ \t]*\\\\begin{\\([^}]+\\)}")
                              (match-string 1))
                             ((looking-at "[ \t]*\\\\\\[") "\\[")
                             ((looking-at "[ \t]*\\$\\$") "$$"))))
              (when env-name
                (let ((env-end (cond
                                ((string= env-name "\\[")
                                 (re-search-forward "\\\\\\]" nil t))
                                ((string= env-name "$$")
                                 (re-search-forward "\\$\\$" nil t))
                                (t
                                 (re-search-forward 
                                  (format "\\\\end{%s}" env-name) nil t)))))
                  (and env-end 
                       (>= current-pos math-start)
                       (<= current-pos env-end)
                       env-name))))))))))

;; === 主要接口函数 ===

(defun laas-org-in-math-p ()
  "判断当前位置是否在数学环境中"
  (or (laas-org--in-org-latex-block-p)
      (laas-org--check-math-with-linebreaks)
      (let* ((pos (point))
             (bound (laas-org--get-search-bound))
             (env-match (laas-org--match-environment bound))
             (macro-match (laas-org--match-macro bound))
             (switch-match (laas-org--match-switch bound))
             (match (cons nil bound))
             math-on)
        
        (when env-match (setq match env-match))
        (when (and macro-match (>= (cdr macro-match) (cdr match)))
          (setq match macro-match))
        (when (and switch-match (>= (cdr switch-match) (cdr match)))
          (setq match switch-match))
        
        (setq math-on (laas-org--is-math-command (car match)))
        
        (unless math-on
          (setq math-on (laas-org--check-togglers bound pos (cdr match))))
        
        math-on)))

(defun laas-org-current-math-env ()
  "获取当前数学环境名称"
  (let* ((bound (laas-org--get-search-bound))
         (env-match (laas-org--match-environment bound))
         (macro-match (laas-org--match-macro bound))
         (switch-match (laas-org--match-switch bound))
         (match nil))
    
    (when env-match (setq match env-match))
    (when (and macro-match 
               (laas-org--is-math-command (car macro-match))
               (or (not match) (>= (cdr macro-match) (cdr match))))
      (setq match macro-match))
    (when (and switch-match 
               (laas-org--is-math-command (car switch-match))
               (or (not match) (>= (cdr switch-match) (cdr match))))
      (setq match switch-match))
    
    (when (and match (laas-org--is-math-command (car match)))
      (car match))))

;; === 专用判断函数 ===

(defun laas-org-in-equation-p ()
  "检测是否在 equation 环境中"
  (let ((env (laas-org-current-math-env)))
    (and env (string-match-p "equation" env))))

(defun laas-org-in-align-p ()
  "检测是否在 align 环境中"
  (let ((env (laas-org-current-math-env)))
    (and env (string-match-p "align" env))))

(defun laas-org-in-matrix-p ()
  "检测是否在矩阵环境中"
  (let ((env (laas-org-current-math-env)))
    (and env (string-match-p "matrix" env))))

(defun laas-org-in-display-math-p ()
  "检测是否在显示数学环境中"
  (let ((env (laas-org-current-math-env)))
    (and env 
         (or (member env '("equation" "equation*" "displaymath"))
             (string-match-p "align\\|gather\\|multline\\|flalign" env)
             (string= env "\\[")
             (string= env "$$")))))

(defun laas-org-in-inline-math-p ()
  "检测是否在内联数学环境中"
  (let ((env (laas-org-current-math-env)))
    (and env (string= env "\\("))))

(defun laas-org-after-linebreak-p ()
  "检测是否在换行符之后"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (or (looking-back "\\\\\\\\[ \t\n]*" (- (point) 20))
        (save-excursion
          (forward-line -1)
          (end-of-line)
          (looking-back "\\\\\\\\[ \t]*" (line-beginning-position))))))

(defun laas-org-in-multiline-math-p ()
  "检测是否在多行数学环境中"
  (and (laas-org-in-math-p)
       (or (laas-org-in-align-p)
           (laas-org-in-display-math-p)
           (laas-org-after-linebreak-p))))

;; === 提供接口 ===
(provide 'laas-org-math-detection)

(message "改进的 LAAS Org 数学环境检测接口加载完成（支持换行符）")
