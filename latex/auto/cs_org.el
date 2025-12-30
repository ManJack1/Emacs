;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "cs_org"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm") ("ctex" "") ("amsmath" "") ("amssymb" "") ("amsfonts" "") ("bm" "") ("mathtools" "") ("tikz" "") ("xcolor" "") ("graphicx" "") ("float" "") ("caption" "") ("hyperref" "") ("minted" "") ("tcolorbox" "") ("titlesec" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbEnv")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimWrite")
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "EscVerb*")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "EscVerb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")
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
    "tcolorbox"
    "titlesec")
   (LaTeX-add-xcolor-definecolors
    "boxblue"
    "boxgreen"
    "boxpurple"
    "boxgray"
    "boxback"
    "boxframe")
   (LaTeX-add-tcolorbox-newtcolorboxes
    '("NOTE" "" "" "")
    '("WARNING" "" "" "")
    '("TIP" "" "" "")
    '("EXAMPLE" "" "" ""))
   (LaTeX-add-tcolorbox-tcbuselibraries
    "listings,skins,breakable"))
 :latex)

