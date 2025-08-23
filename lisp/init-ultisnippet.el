;;; init-ultisnippet.el --- LaTeX auto-activating snippets with straight.el

;; 支持跳转的简单 LAAS 配置
(use-package laas
  :straight t
  :hook (LaTeX-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    ;;:cond #'texmathp 
                    :cond #'laas-org-in-math-p
                    "le" "\\leq"
                    "On" "O(n)"
                    "ra" " \\Rightarrow "
                    "oo"  "\\infty"
                    "int" "\\int"
                    "-oo"  "-\\infty"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    ";;" "\\&"
                    "\\\\" "\\\\\\\\"  ; 需要转义
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum\\limits_{$1}^{$2} $0"))
                    "bmat" (lambda () (interactive)
                             (yas-expand-snippet "\\begin{bmatrix} $1 \\end{bmatrix} $0"))
                    "lc" (lambda () (interactive)
                           (yas-expand-snippet "\\left( $1 \\right) $0"))
                    "xsp" (lambda () (interactive)
                            (yas-expand-snippet "$1^{$2} + $1^{2 $2} + \\dots + $1^{n $2}"))
                    "xas" (lambda () (interactive)
                            (yas-expand-snippet "$1_{1}$2_{1} + $1_{2}$2_{2} + \\dots + $1_{$3}$2_{$3}"))
                    "ff" (lambda () (interactive)
                           (yas-expand-snippet "\\frac{$1}{$2} $0"))
                    "prod" (lambda () (interactive)
                             (yas-expand-snippet "\\prod\\limits_{${1:i}}^{${2:n}} ${0:x}"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; 通用的正则吸取函数，支持 YAS 跳转
(defun my/simple-absorb (pattern replacement)
  "通用的正则吸取函数，支持 YAS 跳转
PATTERN: 正则表达式模式
REPLACEMENT: 替换字符串，用 %s 表示匹配内容，支持 $1, $2, $0 跳转点"
  (cond
   ;; 1. 尝试正则模式匹配吸取
   ((looking-back pattern (line-beginning-position))
    (let ((matched (match-string 1)))
      (replace-match "")
      (yas-expand-snippet (format replacement matched))))
   ;; 2. 尝试 LAAS 对象包装（如果存在的话）
   ((and (fboundp 'laas-object-on-left-condition)
         (laas-object-on-left-condition)
         (string-match "\\\\\\([a-z]+\\)" replacement))
    (laas-wrap-previous-object (match-string 1 replacement)))
   ;; 3. 普通插入
   (t 
    (yas-expand-snippet (replace-regexp-in-string "%s" "" replacement)))))

;; 核心吸取函数
(defun my/absorb-sub ()
  "下标吸取: xsub -> x_{} 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)sub" "%s_{$1}$0"))

(defun my/absorb-pow ()
  "上标吸取: xpow -> x^{} 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)pow" "%s^{$1}$0"))

(defun my/absorb-brace ()
  "括号吸取: xbc -> x() 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)bc" "%s($1)$0"))

(defun my/absorb-bb ()
  "黑板体吸取: Abb -> \\mathbb{A}"
  (interactive)
  (my/simple-absorb "\\([A-Z]\\)bb" "\\mathbb{%s}"))

(defun my/absorb-hat ()
  "帽子吸取: xhat -> \\hat{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)hat" "\\hat{%s}"))

(defun my/absorb-vec ()
  "向量吸取: xvec -> \\vec{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)vec" "\\vec{%s}"))

(defun my/absorb-bar ()
  "横线吸取: xbar -> \\bar{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)bar" "\\bar{%s}"))

(defun my/absorb-dot ()
  "点吸取: xdot -> \\dot{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)dot" "\\dot{%s}"))

(defun my/absorb-ddot ()
  "双点吸取: xddot -> \\ddot{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)ddot" "\\ddot{%s}"))

(defun my/absorb-tilde ()
  "波浪号吸取: xtilde -> \\tilde{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)tilde" "\\tilde{%s}"))

;; 注册到 LAAS
(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
    :cond #'laas-org-in-math-p
    "sub" #'my/absorb-sub
    "bb" #'my/absorb-bb
    "pow" #'my/absorb-pow
    "hat" #'my/absorb-hat
    "bc" #'my/absorb-brace
    "vec" #'my/absorb-vec
    "bar" #'my/absorb-bar
    "dot" #'my/absorb-dot
    "ddot" #'my/absorb-ddot
    "tilde" #'my/absorb-tilde))

;; 额外的数学符号快速输入
(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    ;; 希腊字母
    "alpha" "\\alpha"
    "beta" "\\beta"
    "gamma" "\\gamma"
    "delta" "\\delta"
    "epsilon" "\\epsilon"
    "zeta" "\\zeta"
    "eta" "\\eta"
    "theta" "\\theta"
    "iota" "\\iota"
    "kappa" "\\kappa"
    "lambda" "\\lambda"
    "mu" "\\mu"
    "nu" "\\nu"
    "xi" "\\xi"
    "pi" "\\pi"
    "rho" "\\rho"
    "sigma" "\\sigma"
    "tau" "\\tau"
    "upsilon" "\\upsilon"
    "phi" "\\phi"
    "chi" "\\chi"
    "psi" "\\psi"
    "omega" "\\omega"
    
    ;; 大写希腊字母
    "Gamma" "\\Gamma"
    "Delta" "\\Delta"
    "Theta" "\\Theta"
    "Lambda" "\\Lambda"
    "Xi" "\\Xi"
    "Pi" "\\Pi"
    "Sigma" "\\Sigma"
    "Upsilon" "\\Upsilon"
    "Phi" "\\Phi"
    "Psi" "\\Psi"
    "Omega" "\\Omega"
    
    ;; 数学运算符
    "neq" "\\neq"
    "geq" "\\geq"
    "leq" "\\leq"
    "approx" "\\approx"
    "equiv" "\\equiv"
    "pm" "\\pm"
    "mp" "\\mp"
    "times" "\\times"
    "div" "\\div"
    "cdot" "\\cdot"
    "partial" "\\partial"
    "nabla" "\\nabla"
    "exists" "\\exists"
    "forall" "\\forall"
    "in" "\\in"
    "notin" "\\notin"
    "subset" "\\subset"
    "supset" "\\supset"
    "subseteq" "\\subseteq"
    "supseteq" "\\supseteq"
    "cup" "\\cup"
    "cap" "\\cap"
    "emptyset" "\\emptyset"
    
    ;; 箭头
    "to" "\\to"
    "mapsto" "\\mapsto"
    "leftarrow" "\\leftarrow"
    "rightarrow" "\\rightarrow"
    "leftrightarrow" "\\leftrightarrow"
    "Leftarrow" "\\Leftarrow"
    "Rightarrow" "\\Rightarrow"
    "Leftrightarrow" "\\Leftrightarrow"
    
    ;; 函数模板
    "lim" (lambda () (interactive)
            (yas-expand-snippet "\\lim_{$1 \\to $2} $0"))
    "limsup" (lambda () (interactive)
               (yas-expand-snippet "\\limsup_{$1 \\to $2} $0"))
    "liminf" (lambda () (interactive)
               (yas-expand-snippet "\\liminf_{$1 \\to $2} $0"))))

;; Org-mode 集成
(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              ;; 在 org-mode 中也启用 laas
              (when (fboundp 'laas-mode)
                (laas-mode 1)))))

(provide 'init-ultisnippet)
;;; init-ultisnippet.el ends here
