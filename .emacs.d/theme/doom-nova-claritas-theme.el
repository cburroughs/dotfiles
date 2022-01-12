;; doom-vibrant-theme.el --- a more vibrant version of doom-one -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2016-2021 Henrik Lissner
;;
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Created: December 6, 2020
;; Version: 2.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; A version of `doom-one' that uses more vibrant colors.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Theme definition

(def-doom-theme doom-nova-claritas
  "A dark theme based off of doom-one with more clarity colors."

  ;; name        gui       256           16
  ((bg         '("#242730"))
   (fg         '("#bbc2cf"))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#2a2e38"))
   (fg-alt     '("#5D656B"))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
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
   (comments       base5)
   (doc-comments   (doom-lighten base4 0.3))
   (constants      violet)
   (functions      cyan)
   (keywords       blue)
   (methods        violet)
   (operators      magenta)
   (type           yellow)
   (strings        green)
   (variables      base8)
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

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-nova-claritas--theme.el ends here
