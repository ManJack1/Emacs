;; 支持跳转的简单 LAAS 吸取函数


(use-package laas
  :straight t
  :hook (LaTeX-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
		    "le" "\\leq"
                    "On" "O(n)"
		    "ra" " \\Rightarrow "
		    "oo"  "\\infty"
		    "int" "\\int"
		    "-oo"  "-\\infty"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
		    ";;" "\&"
		    "\\" "\\\\"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum\\limits_{$1^{$2} $0"))

                    "bmat" (lambda () (interactive)
                            (yas-expand-snippet "\\begin{bmatrix} $1 \\end{bmatrix} $0"))

                    "lc" (lambda () (interactive)
                            (yas-expand-snippet "\\left( $1 \\right) $0"))

                    "xsp" (lambda () (interactive)
                            (yas-expand-snippet "$1^{$2} + $1^{2 $2} + \\dots + $1^{n $2}"))

                    "xas" (lambda () (interactive)
                            (yas-expand-snippet "$1_{1}$2_{1} + $1_{2}$2_{2} + \\dots + $1_{$3}$2_{$3}"))
                    "ff" (lambda () (interactive)
                            (yas-expand-snippet "\\frac{$1{$2} $0"))

                    "prod" (lambda () (interactive)
                            (yas-expand-snippet "\\prod\\limits_{${1:i}^{${2:n}}${0:x}"))

                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

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
  (my/simple-absorb "\\([a-zA-Z]+\\)sub" "%s_{$1$0"))


(defun my/absorb-pow ()
  "下标吸取: xpow -> x^{} 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)pow" "%s^{$1$0"))


(defun my/absorb-brace ()
  "xbc -> x() 支持跳转"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)bc" "%s($0"))

(defun my/absorb-bb ()
  "黑板体吸取: Abb -> \\mathbb{A}"
  (interactive)
  (my/simple-absorb "\\([A-Z]\\)bb" "\\mathbbr{$s"))

(defun my/absorb-hat ()
  "帽子吸取: xhat -> \\hat{x}"
  (interactive)
  (my/simple-absorb "\\([a-zA-Z]+\\)hat" "\\hat{$s"))

;; 注册到 LAAS
(with-eval-after-load 'laas
  (aas-set-snippets 'laas-mode
    :cond #'texmathp
    "sub" #'my/absorb-sub
    "bb" #'my/absorb-bb
    "pow" #'my/absorb-pow
    "hat" #'my/absorb-hat
    "bc" #'my/absorb-brace))



(provide 'init-ultisnippet)
