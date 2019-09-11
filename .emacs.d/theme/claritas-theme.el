;;; claritas-theme.el --- claritas theme

;; Copyright (C) 2003 by Richard Wellum
;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A personal custimzation of the clarity theme.  (Built on the port of clarity
;; from from `color-themes')

;;; Code:

;; for lighten/darken
(require 'doom-themes)

;; NOTE: "palette" library is already pre-loaded


(deftheme claritas
  "claritas theme")

(custom-theme-set-faces
 'claritas

 '(default ((t (:background "black" :foreground "white"))))
 '(mouse ((t (:foregound "white"))))
 '(cursor ((t (:foregound "yellow"))))
 '(border ((t (:foregound "white"))))

 '(help-highlight-face ((t (:underline t))))
 '(list-matching-lines-face ((t (:bold t :weight bold))))
 '(view-highlight-face ((t (:background "DarkOliveGreen"))))
 '(widget-mouse-face ((t (:background "DarkOliveGreen"))))

 '(CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))
 '(CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))
 '(CUA-rectangle-noselect-face ((t (:background "DimGray" :foreground "white"))))
 '(bold ((t (:bold t :weight bold))))
 '(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
 '(border ((t (:background "white"))))
 '(clearcase-dired-checkedout-face ((t (:foreground "red"))))
 '(comint-highlight-input ((t (:bold t :weight bold))))
 '(comint-highlight-prompt ((t (:foreground "cyan"))))
 '(cursor ((t (:background "yellow"))))
 ;; csb: Hacky work around to avoid this ending up as some jarrying
 ;; courier-esque thing when everything is already monospaced
 '(fixed-pitch ((t (:family "DejaVu Sans Mono"))))
 '(flash-paren-face-off ((t (nil))))
 '(flash-paren-face-on ((t (nil))))
 '(flash-paren-face-region ((t (nil))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "OrangeRed"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(ibuffer-dired-buffer-face ((t (:foreground "LightSkyBlue"))))
 '(ibuffer-help-buffer-face ((t (:foreground "OrangeRed"))))
 '(ibuffer-hidden-buffer-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(ibuffer-occur-match-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(ibuffer-read-only-buffer-face ((t (:foreground "PaleGreen"))))
 '(ibuffer-special-buffer-face ((t (:foreground "Cyan"))))
 '(ibuffer-title-face ((t (:foreground "PaleGreen"))))
 '(fringe ((t (:background "grey10"))))
 '(header-line ((t (:box (:line-width -1 :style released-button) :foreground "grey20" :background "grey90" :box nil))))
 '(highlight ((t (:background "DarkOliveGreen"))))
 '(ibuffer-deletion-face ((t (:foreground "red"))))
 '(ibuffer-marked-face ((t (:foreground "green"))))
 '(isearch ((t (:background "PaleVioletRed2" :foreground "brown4"))))
 '(isearch-lazy-highlight-face ((t (:background "PaleTurquoise4"))))
 '(italic ((t (:italic t :slant italic))))
 '(menu ((t (nil))))
 '(mode-line ((t (:foreground "yellow" :background "DarkSlateBlue"
                  :box (:line-width -1 :style released-button)))))
 '(mouse ((t (:background "white"))))
 '(region ((t (:background "blue"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "DarkSlateBlue"))))
 '(show-block-face1 ((t (:background "gray10"))))
 '(show-block-face2 ((t (:background "gray15"))))
 '(show-block-face3 ((t (:background "gray20"))))
 '(show-block-face4 ((t (:background "gray25"))))
 '(show-block-face5 ((t (:background "gray30"))))
 '(show-block-face6 ((t (:background "gray35"))))
 '(show-block-face7 ((t (:background "gray40"))))
 '(show-block-face8 ((t (:background "gray45"))))
 '(show-block-face9 ((t (:background "gray50"))))
 '(show-paren-match-face ((t (:background "turquoise"))))
 '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
 '(tool-bar ((t (:background "grey75" :foreground "black"
                 :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:background "LightYellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(underline ((t (:underline t))))
 '(variable-pitch ((t (:family "helv"))))
 '(widget-button-face ((t (:bold t :weight bold))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "LimeGreen"))))
 '(widget-field-face ((t (:background "DimGray"))))
 '(widget-inactive-face ((t (:foreground "LightGray"))))
 '(widget-single-line-field-face ((t (:background "DimGray"))))

;; hightlighting lines
 '(hl-line ((t (:background "gray18"))))
 '(swiper-line-face ((t (:background "gray28"))))

 ;; centaur-tabs
 '(centaur-tabs-default ((t (:background "black" :foreground "black"))))
 `(centaur-tabs-selected ((t (:weight bold :height 0.91
                                      :background "#31343E" :foreground ,palette/monokai/cyan-d))))
 '(centaur-tabs-unselected ((t (:height 0.91
                                      :background "#3D3C3D":foreground "grey50"))))
 `(centaur-tabs-selected-modified ((t (:inherit centaur-tabs-selected))))
 '(centaur-tabs-unselected-modified ((t (:inherit 'centaur-tabs-unselected))))
 '(centaur-tabs-active-bar-face ((t :background "black")))

   ;; company-mode
 `(company-tooltip ((t (:background ,palette/monokai/highlight-line
                                   :foreground ,palette/monokai/emphasis))))
 `(company-tooltip-selection ((t (:background ,palette/monokai/blue
                                              :foreground ,palette/monokai/background))))
 `(company-tooltip-mouse ((t (:background ,palette/monokai/blue
                                          :foreground ,palette/monokai/background))))
 `(company-tooltip-common ((t (:foreground ,palette/monokai/blue
                                           :underline t))))
 `(company-tooltip-common-selection ((t (:foreground ,palette/monokai/background
                                                     :background ,palette/monokai/blue
                                                     :underline t))))
 `(company-preview ((t (:background ,palette/monokai/highlight-line
                                    :foreground ,palette/monokai/emphasis))))
 `(company-preview-common ((t (:foreground ,palette/monokai/blue
                                           :underline t))))
 `(company-scrollbar-bg ((t (:background ,palette/monokai/gray))))
 `(company-scrollbar-fg((t (:background ,palette/monokai/comments))))
 `(company-tooltip-annotation ((t (:background ,palette/monokai/highlight-line
                                               :foreground ,palette/monokai/green))))
 `(company-template-field ((t (:background ,palette/monokai/highlight-line
                                           :foreground ,palette/monokai/blue))))

 ;; outline and org mode
 '(outline-1 ((t (:foreground "LightSkyBlue"))))
 '(outline-2 ((t (:foreground "LightGoldenrod"))))
 `(outline-3 ((t (:foreground ,palette/monokai/green))))
 `(outline-4 ((t (:foreground ,palette/monokai/orange))))
 `(outline-5 ((t (:foreground ,palette/monokai/blue))))
 `(outline-6 ((t (:foreground ,palette/monokai/red-d))))
 `(outline-6 ((t (:foreground ,(doom-lighten palette/monokai/green 0.25)))))
 `(outline-7 ((t (:foreground ,(doom-lighten palette/monokai/orange 0.25)))))
 `(outline-8 ((t (:foreground ,(doom-lighten palette/monokai/violet 0.25)))))

 ;; rainbow-delimiters
 ;; a rainbow, but with subtle earthy instead of psychedelic tones
 `(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,palette/srcery/magenta))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,palette/srcery/blue))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,palette/srcery/green))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,palette/srcery/yellow))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,palette/srcery/orange))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,palette/srcery/white))))
 `(rainbow-delimiters-depth-8-face ((t (:foreground ,palette/srcery/bright-magenta))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground ,palette/srcery/bright-blue))))
 `(rainbow-delimiters-depth-10-face ((t (:foreground ,palette/srcery/bright-green))))
 `(rainbow-delimiters-depth-11-face ((t (:foreground ,palette/srcery/bright-yellow))))
 `(rainbow-delimiters-depth-12-face ((t (:foreground ,palette/srcery/bright-orange))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground ,palette/monokai/red :weight bold
                                                      :inverse-video t))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit 'rainbow-delimiters-unmatched-face))))
)




;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'claritas)

;;; claritas-theme.el ends here
