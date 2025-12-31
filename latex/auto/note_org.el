;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "note_org"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "UTF8" "fontset=fandol" "")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm") ("ctex" "" "UTF8" "fontset=fandol") ("hyperref" "") ("mathrsfs" "") ("amsmath" "") ("amssymb" "") ("amsthm" "") ("mathtools" "") ("tikz" "") ("xcolor" "dvipsnames") ("tcolorbox" "most") ("minted" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimWrite")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbEnv")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "EscVerb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "EscVerb*")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "ctex"
    "hyperref"
    "mathrsfs"
    "amsmath"
    "amssymb"
    "amsthm"
    "mathtools"
    "tikz"
    "xcolor"
    "tcolorbox"
    "minted")
   (LaTeX-add-tcolorbox-newtcolorboxes
    '("definition" "1" "[" "")
    '("theorem" "1" "[" "")
    '("lemma" "1" "[" "")
    '("proposition" "1" "[" "")
    '("corollary" "1" "[" "")
    '("ep" "1" "[" "")
    '("note" "1" "[" "")))
 :latex)

