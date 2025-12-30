;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "cs"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm") ("ctex" "") ("amsmath" "") ("amssymb" "") ("amsfonts" "") ("bm" "") ("mathtools" "") ("tikz" "") ("xcolor" "") ("graphicx" "") ("float" "") ("caption" "") ("hyperref" "") ("minted" "") ("tcolorbox" "")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "ctex"
    "amsmath"
    "amssymb"
    "amsfonts"
    "bm"
    "mathtools"
    "tikz"
    "xcolor"
    "graphicx"
    "float"
    "caption"
    "hyperref"
    "minted"
    "tcolorbox"))
 :latex)

