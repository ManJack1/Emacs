;;; doom-everforest-theme.el --- inspired by Everforest -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Claude
;; Maintainer: Claude
;; Source: https://github.com/sainnhe/everforest
;;
;;; Commentary:
;;
;; A green based color scheme inspired by forest.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-everforest-theme nil
  "Options for the `doom-everforest' theme."
  :group 'doom-themes)

(defcustom doom-everforest-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-comment-bg doom-everforest-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'doom-everforest-theme
  :type 'boolean)

(defcustom doom-everforest-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-everforest-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-everforest
    "A dark theme inspired by Everforest."
  :family 'doom-everforest
  :background-mode 'dark

  ;; name        default   256           16
  ((bg         '("#2b3339" "#2b3339"    "black"  ))
   (fg         '("#d3c6aa" "#d3c6aa"    "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#232a2e" "#232a2e"    "black"        ))
   (fg-alt     '("#859289" "#859289"    "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1e2326" "#1e2326"    "black"        ))
   (base1      '("#272e33" "#272e33"    "brightblack"  ))
   (base2      '("#2e383c" "#2e383c"    "brightblack"  ))
   (base3      '("#374145" "#374145"    "brightblack"  ))
   (base4      '("#414b50" "#414b50"    "brightblack"  ))
   (base5      '("#859289" "#859289"    "brightblack"  ))
   (base6      '("#9da9a0" "#9da9a0"    "brightblack"  ))
   (base7      '("#b4c0b8" "#b4c0b8"    "brightblack"  ))
   (base8      '("#d3c6aa" "#d3c6aa"    "white"        ))

   (grey       base4)
   (red        '("#e67e80" "#e67e80"    "red"          ))
   (orange     '("#e69875" "#e69875"    "brightred"    ))
   (green      '("#a7c080" "#a7c080"    "green"        ))
   (teal       '("#83c092" "#83c092"    "brightgreen"  ))
   (yellow     '("#dbbc7f" "#dbbc7f"    "yellow"       ))
   (blue       '("#7fbbb3" "#7fbbb3"    "brightblue"   ))
   (dark-blue  '("#5a8c9d" "#5a8c9d"    "blue"         ))
   (magenta    '("#d699b6" "#d699b6"    "brightmagenta"))
   (violet     '("#d699b6" "#d699b6"    "magenta"      ))
   (cyan       '("#83c092" "#83c092"    "brightcyan"   ))
   (dark-cyan  '("#5a8f82" "#5a8f82"    "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      green)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-everforest-brighter-comments fg-alt (doom-darken fg-alt 0.2)))
   (doc-comments   (doom-lighten (if doom-everforest-brighter-comments fg-alt (doom-darken fg-alt 0.2)) 0.25))
   (constants      violet)
   (functions      green)
   (keywords       red)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        teal)
   (variables      fg)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-everforest-brighter-modeline
				 (doom-darken green 0.45)
			       (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-everforest-brighter-modeline
				 (doom-darken green 0.475)
			       `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-everforest-padded-modeline
      (if (integerp doom-everforest-padded-modeline) doom-everforest-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground yellow)
   ((font-lock-comment-face &override)
    :background (if doom-everforest-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-everforest-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-everforest-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; org-mode
   (org-block :background (doom-darken bg 0.1))
   (org-block-begin-line :foreground base5 :slant 'italic)
   (org-block-end-line :foreground base5 :slant 'italic)
   (org-level-1 :foreground red :weight 'bold :height 1.3)
   (org-level-2 :foreground orange :weight 'bold :height 1.2)
   (org-level-3 :foreground yellow :weight 'bold :height 1.1)
   (org-level-4 :foreground green :weight 'bold)
   (org-level-5 :foreground teal)
   (org-level-6 :foreground blue)
   (org-level-7 :foreground violet)
   (org-level-8 :foreground base6)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-everforest-theme.el ends here
