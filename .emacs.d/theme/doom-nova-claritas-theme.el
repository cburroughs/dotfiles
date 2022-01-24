;; doom-vibrant-theme.el --- a more vibrant version of doom-one -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2022 Chris Burroughs
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2003 by Richard Wellum
;; Copyright (C) 2016-2021 Henrik Lissner
;;
;; Author: 
;; Created: 
;; Version: 
;; Keywords: custom themes, faces
;; Homepage: 
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; A decade later visual refresh drawing from a personal customization of the
;; clarity theme, built on the port of clarity from from `color-themes'.  Then
;; smarshed into doom-vibrant with a bunch of other fun colors thrown in.
;;
;;; Code:

(require 'doom-themes)
(require 'csb/palette)


;;
;;; Theme definition

(def-doom-theme doom-nova-claritas
  "A vibrant clarity monstrosity."

  ;; I've traditional used white text with a black background.  There isn't any
  ;; great evidence base for choosing here -- and it is unclear how applicable
  ;; 1980s studies on monochrome CRTs would be to modern LCDs -- but it is
  ;; plausible that pure white on pure black is not ideal on modern screens,
  ;; even if it was the cool option on older displays.  See
  ;; <https://uxmovement.com/content/why-you-should-never-use-pure-black-for-text-or-backgrounds/>
  ;; for an explicit argument along those lines.  For an example of generally
  ;; inconclusive anecdotes see
  ;; <https://stackoverflow.com/questions/2985174/programming-with-white-text-on-black-background>
  ;; or
  ;; <https://tatham.blog/2008/10/13/why-light-text-on-dark-background-is-a-bad-idea/>

  ;; General color stuff: https://notes.neeasade.net/color-spaces.html

  ;; However! Many modern themes have so little contrast between their
  ;; dark-but-not-pure-black and kinda-gray-ish white that I can not for the
  ;; life of me get use to them.  Using an even lighter shade of gray for the
  ;; comments is also common, but comments are important!

  ;; https://contrastchecker.online/ Contrast Ratio (WCAG AAA minimum is 7:1)
  ;; Pure #ffffff on #00000 : 21:1
  ;; doom-one:7.82:1
  ;; monokai: 13.94:1 
  ;; doom-vibrant: 8.33:1 
  ;; tomorrow:  9.8:1
  ;; spacemacs:  6.69:1 
  ;; solarized (dark): 4.74:1 

  ;; Contrast 12.22:1, symetric 0x20 offset
  ;;'(default ((t (:background "#202020" :foreground "#dfdfdf"))))
  ;; Contrast  8.47:1 , symetric 0x30 offset
  ;;'(default ((t (:background "#303030" :foreground "#cfcfcf"))))
  ;; Contrast 11.21:1 , symetric 0x24 offset
  ;;'(default ((t (:background "#242424" :foreground "#dbdbdb"))))

  ;;`(default ((t (:background ,palette/monokai/background
  ;;:foreground ,palette/monokai/foreground))))

  ;; doom-vibrant
  ;; '(default ((t (:background "#242730" :foreground "#bbc2cf"))))
  ;; vibrant +- 8
  ;; '(default ((t (:background "#1c1f28" :foreground "#c3cad7"))))
  ;; vibrant +- 4
  ;;'(default ((t (:background "#20232c" :foreground "#bfc6d3"))))

  ;; Comically after trying all of the above, I have gone with "doom-vibrant
  ;; but with slightly more contrast"

  ;; name        gui       256           16
  ((bg         '("#1c1f28")) ;; doom-vibrant +- 8
   (fg         '("#c3cad7")) ;; contrast  9.99:1 

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   ;; Also +- from doom-vibrant defaults for more contrast
   (bg-alt     '("#222630"))
   (fg-alt     '("#656d73"))

   ;; These should represent a spectrum from bg to fg, where base0 is a
   ;; starker bg and base8 is a starker fg. For example, if bg is light grey
   ;; and fg is dark grey, base0 should be white and base8 should be black.
   ;; NOTE: Uncnaged definitions from doom-vibrant, brought in other colors
   ;; later as needed.
   (base0      '("#1c1f24"))
   (base1      '("#1c1f24"))
   (base2      '("#21272d"))
   (base3      '("#23272e"))
   (base4      '("#484854"))
   (base5      '("#62686E"))
   (base6      '("#757B80"))
   (base7      '("#9ca0a4"))
   (base8      '("#DFDFDF"))

   (grey       base4)
   (red        '("#ff665c"))
   (orange     '("#e69055"))
   (green      '("#7bc275"))
   (teal       '("#4db5bd"))
   (yellow     '("#FCCE7B"))
   (blue       '("#51afef"))
   (dark-blue  '("#1f5582"))
   (magenta    '("#C57BDB"))
   (violet     '("#a991f1"))
   (cyan       '("#5cEfFF"))
   (dark-cyan  '("#6A8FBF"))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   base0)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       "OrangeRed")
   (doc-comments   (doom-lighten "LightSalmon" 0.1))
   (constants      teal) ; was violet
   (functions      blue) ; was cyan
   (keywords       cyan) ; was blue
   (methods        violet)
   (operators      magenta)
   (type           green) ; was yellow
   (strings        "LightSalmon") ; was green
   (variables      yellow) ; was base8
   (numbers        orange)
   (region         "#3d4451")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))

  ;; These are extra color variables used only in this theme; i.e. they aren't
  ;; mandatory for derived themes.

  ;;;; Base theme face overrides
  (
   ;; + 12 over background (vibrant +- 8)  (contrast with text drops to 8.58:1 )
   (hl-line :background "#282b34")
   (lin-hl :background (doom-darken region 0.1))
   (mct-highlight-candidate :background (doom-darken region 0.1))

   (cursor :background palette/monokai/yellow)

   ;; Contrast   8.25:1
   (mode-line :foreground palette/monokai/yellow-l :background "DarkSlateBlue")
   (mode-line-inactive :foreground "grey80" :background "grey30")
   ;; TODO mlscroll?
   
   ;; outline and org mode
   (outline-1 :foreground "LightSkyBlue")
   (outline-2 :foreground "LightGoldenrod")
   (outline-3 :foreground palette/monokai/cyan)
   (outline-4 :foreground palette/monokai/orange)
   (outline-5 :foreground palette/monokai/magenta)
   (outline-6 :foreground palette/monokai/yellow) ; TODO: change re cursor?
   (outline-7 :foreground palette/monokai/violet)
   (outline-8 :foreground palette/monokai/orange-l)

   (org-priority :foreground palette/doom-one/cyan)
   (org-todo :foreground palette/doom-one/red)
   (org-warning :foreground palette/doom-one/orange)
   (org-upcoming-deadline :foreground palette/doom-one/yellow)
   (org-done :foreground palette/doom-one/green)

   (org-date :foreground (doom-lighten blue 0.25))
   
   ;; rainbow-delimiters
   ;; a rainbow, but with subtle earthy instead of psychedelic tones
   (rainbow-delimiters-depth-1-face :foreground fg)
   (rainbow-delimiters-depth-2-face :foreground palette/srcery/magenta)
   (rainbow-delimiters-depth-3-face :foreground palette/srcery/blue)
   (rainbow-delimiters-depth-4-face :foreground palette/srcery/green)
   (rainbow-delimiters-depth-5-face :foreground palette/srcery/orange)
   (rainbow-delimiters-depth-6-face :foreground palette/srcery/white)
   (rainbow-delimiters-depth-7-face :foreground palette/srcery/bright-magenta)
   (rainbow-delimiters-depth-8-face :foreground palette/srcery/bright-blue)
   (rainbow-delimiters-depth-9-face :foreground palette/srcery/bright-green)
   (rainbow-delimiters-depth-10-face :foreground palette/srcery/bright-orange)
   (rainbow-delimiters-depth-11-face :foreground palette/srcery/bright-white)

   ;; TODO: Investiage paren-face package, or at least harmony with it
   (paren-face-match    :background orange :weight 'ultra-bold)
   (paren-face-mismatch :foreground base0 :background red :weight 'ultra-bold)

   (all-the-icons-ibuffer-size-face :foreground violet)
   (all-the-icons-ibuffer-mode-face :foreground fg)
   (all-the-icons-ibuffer-file-face :foreground base7)

   (completions-annotations :foreground base7)
   (corfu-current :background (doom-darken region 0.1))

   (isearch :background (doom-darken orange 0.15) :foreground base8)
   (ctrlf-highlight-line :inherit 'lin-hl)
   )

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-nova-claritas-theme.el ends here
