;;; palory-theme.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

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

;;; Commentary: palory theme, for programmer

;;; Code:

;;; base on zenburn and solarized
;; https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el and
;; https://github.com/sellout/emacs-color-theme-solarized

;; the easy way to get following vlaue is: `customize face' => Apply and save => copy and paste


(deftheme palory "The Palory color theme. Base on zenburn and solarized")

(let ((base03  "#002b36")
      (base02  "#073642")
      (base01  "#586e75")
      (base00  "#657b83")
      (base0   "#839496")
      (base1   "#93a1a1")
      (base2   "#eee8d5")
      (base3   "#fdf6e3")
      (yellow  "#b58900")
      (orange  "#cb4b16")
      (red     "#dc322f")
      (magenta "#d33682")
      (violet  "#6c71c4")
      (blue    "#268bd2")
      (cyan    "#2aa198")
      (green   "#859900"))

  (custom-theme-set-faces
   'palory
   ;; basic coloring
   `(default ((t (:background ,base03 :foreground ,base0))))
   '(cursor ((t (:foreground unspecified :background "#cd0000"))))

   ;; mode-line
   '(mode-line-buffer-id ((t (:foreground "#90377d"))))
   '(mode-line-emphasis ((t (:foreground "Magenta"))))
   `(mode-line ((t (:foreground ,base1 :background ,base02 :box nil))))
   `(mode-line-inactive ((t (:foreground ,base0  :background ,base03 :box nil))))
   '(mode-line-highlight ((t (:box nil))))

   '(minibuffer-prompt ((t (:foreground "#ff8c00" :weight bold))))
   `(fringe ((t (:foreground "green" :background ,base02))))
   `(region ((t (:foreground unspecified :background ,base02))))
   '(secondary-selection ((t (:foreground unspecified :background "#485c60"))))

   ;; `(vertical-border ((t (:foreground ,base2))))

   ;; tooltip
   '(tooltip ((t (:foreground "#111111" :background "#bcc8dd"))))

   ;; toolbar
   '(tool-bar ((t (:background "DarkSlateGrey"))))

   ;; `(header-line ((t (:foreground ,base2 :background ,base02))))

   ;; highlight-symbol
   `(highlight-symbol-face ((t (:background "dodgerblue3" :foreground ,base0))))

   ;; highlight
   '(highlight ((t (:foreground unspecified :foreground "#66e2e2" :background "#454545")))) ; use `unspecified' or `nil' both ok

   ;; link
   '(link ((t (:foreground "#33b2ef" :background unspecified :underline "#33b2ef" weight bold))))
   '(link-visited ((t (:foreground "violet" :background unspecified :underline t :weight normal))))

   ;; info
   '(info-xref ((t (:foreground "DeepSkyBlue2" :weight bold :underline nil))))
   '(info-xref-visited ((t (:inherit info-xref :weight normal))))
   '(info-header-xref ((t (:inherit info-xref))))
   '(info-menu-star ((t (:foreground "#dfaf8f" :weight bold))))
   '(info-menu-5 ((t (:foreground "#df998f"))))
   '(info-node ((t (:foreground "DodgerBlue1" :weight bold))))
   '(info-title-1 ((t (:foreground "green1"))))
   '(info-title-2 ((t (:foreground "green2"))))
   '(info-title-3 ((t (:foreground "green3"))))
   '(info-title-4 ((t (:foreground "DodgerBlue1"))))
   '(info-menu-header ((t (:foreground "LawnGreen"))))
   '(info-header-node ((t (:weight normal))))

   ;; show-paren
   '(show-paren-match ((t (:foreground "SteelBlue3"))))
   '(show-paren-mismatch ((t (:foreground "white" :background "purple"))))

   ;; term
   '(term-color-blue ((t (:foreground "DeepSkyBlue4"))))

   ;; adoc-mode
   '(markup-meta-face ((t (:height 120 :foreground "#999999"))))
   '(markup-meta-hide-face ((t (:height 0.9 :foreground "#777777"))))
   '(adoc-table-del ((t (:background "MidnightBlue" :foreground "eeeeee"))))

   ;; org-mode
   '(org-headline-done ((t (:strike-through t))))
   '(org-done ((t (:strike-through t))))
   `(org-hide ((t (:foreground ,base03))))
   '(org-level-1 ((t (:foreground "lightcoral"))))
   '(org-level-2 ((t (:foreground "dodgerblue2"))))
   '(org-level-3 ((t (:foreground "darkolivegreen2"))))
   '(org-level-4 ((t (:foreground "lightsteelblue"))))
   '(org-level-5 ((t (:foreground "cyan4"))))
   '(org-level-6 ((t (:foreground "Aquamarine"))))
   '(org-level-7 ((t (:foreground "LightSteelBlue"))))
   '(org-level-8 ((t (:foreground "LightSalmon"))))
   '(org-habit-alert-face ((t (:foreground "#228822" :background "gold"))))
   `(org-agenda-structure ((t (:foreground "LightGoldenrod" :background ,base02))))
   '(org-scheduled ((t (:foreground "CadetBlue"))))
   '(org-scheduled-today ((t (:foreground "turquoise2"))))
   '(org-agenda-calendar-sexp ((t (:foreground "MistyRose3"))))


   ;; font lock
   '(font-lock-builtin-face ((t (:foreground "chartreuse3"))))
   `(font-lock-comment-face ((t (:foreground ,base01))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base1))))
   '(font-lock-constant-face ((t (:foreground "#22ccee"))))
   '(font-lock-doc-face ((t (:foreground "turquoise4"))))
   '(font-lock-function-name-face ((t (:foreground "LightCoral"))))
   '(font-lock-keyword-face ((t (:foreground "DarkOliveGreen2"))))
   `(font-lock-negation-char-face ((t (:foreground ,yellow :weight bold))))
   '(font-lock-preprocessor-face ((t (:foreground "#79C96D"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,green :weight bold))))
   '(font-lock-string-face ((t (:foreground "#a800a8"))))
   '(font-lock-type-face ((t (:foreground "#4492e1"))))
   '(font-lock-variable-name-face ((t (:foreground "DodgerBlue2"))))
   '(font-lock-warning-face ((t (:foreground "Pink"))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;; icompletep
   '(icompletep-keys ((t (:foreground "LawnGreen"))))
   '(icompletep-determined ((t (:foreground "DarkMagenta"))))
   '(icompletep-choices ((t (:foreground "burlywood"))))

   ;; ido
   '(ido-subdir ((t (:foreground "#1e90ff" :weight bold))))
   '(ido-first-match ((t (:foreground "DarkMagenta" :weight bold))))
   '(ido-virtual ((t (:foreground "#638383"))))

   ;; anything
   '(anything-ff-directory ((t (:background "222222"))))

   ;; woman
   '(woman-addition ((t (:foreground "DarkMagenta" :weight bold))))
   '(woman-bold ((t (:foreground "cyan4" :weight bold))))
   '(woman-italic ((t (:foreground "orange2" :underline nil))))
   '(woman-unknown ((t (:foreground "LightSalmon2"))))

   ;; hex mode
   '(hexl-address-region ((t (:foreground "#99bb99" :background "#002299"))))
   '(hexl-ascii-region ((t (:foreground "#00cbcb" :background "#334444"))))

   ;; haskell mode
   '(haskell-interactive-face-prompt ((t (:foreground "#00b2ee"))))

   ;; dictionary
   '(dictionary-word-entry-face ((t (:foreground "magenta"))))

   ;; ace-jump-mode
   `(ace-jump-face-background
     ((t (:foreground ,base01 :background ,base03 :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,magenta :background ,base03 :inverse-video nil))))

   `(ac-candidate-face ((t (:background "#dbdbdb" :foreground "black" :underline nil))))
   `(ac-selection-face ((t (:background "SteelBlue" :foreground "white"))))
   `(ac-completion-face ((t (:background "violet" :foreground "black"))))
   '(popup-tip-face ((t (:background "#f9f69d" :foreground "#232323"))))
   `(popup-scroll-bar-foreground-face ((t (:background "#909090"))))
   `(popup-scroll-bar-background-face ((t (:background "#d0d0d0"))))
   `(popup-isearch-match ((t (:background ,base03 :foreground ,base03))))
   `(popup-menu-selection-face ((t (:background "#0000ff" :foreground ,base03))))

   ;; cedet
   '(pulse-highlight-start-face ((t (:background "#222222"))))
   '(semantic-tag-boundary-face ((t (:overline "#227777"))))
   '(semantic-decoration-on-private-members-face ((t (:background "#333333"))))
   '(semantic-decoration-on-unparsed-includes ((t (:background "#555555"))))

   ;; volatile highlights
   '(vhl/default-face ((t (:background "#332244"))))

   ;; hl-line
   `(hl-line ((t (:background ,base02))))

   ;; col-highlight
   `(col-highlight ((t (:background ,base02))))

   ;; comint
   '(comint-highlight-prompt ((t (:foreground "#eedd82" :weight bold))))

   ;; flymake
   '(flymake-warnline ((t (:background "#333300" :foreground "#ccccff"))))
   '(flymake-errline ((t (:background "#402222" :foreground "#cceecc"))))

   ;; git-gutter-fringe
   '(git-gutter-fr:modified ((t (:foreground "Magenta"))))
   '(git-gutter-fr:added ((t (:foreground "cyan"))))
   '(git-gutter-fr:deleted ((t (:foreground "white"))))

   ;; structured-haskell-mode
   '(shm-current-face ((t (:background "#222222"))))
   '(shm-quarantine-face ((t (:background "#262230"))))

   ;; linum
   '(linum ((t (:background "#102235" :foreground "#607b8b"))))

   ;; guide-key
   '(guide-key/key-face ((t (:foreground "violet"))))
   '(guide-key/highlight-command-face ((t (:foreground "#f9f69d"))))

   ;; rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#93e0e3"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#f0dfaf"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#94bff3"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#dca3a3"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#8fb28f"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#8cd0d3"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#dfaf8f"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#dc8cc3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#d0bf8f")))))


  ;;; custom theme variables
  (custom-theme-set-variables
   'palory
   `(ansi-color-names-vector ['bg ,red ,green ,yellow ,blue ,magenta ,cyan ,base0])
   ;; fill-column-indicator
  `(fci-rule-color ,base01)))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))



(provide-theme 'palory)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

(add-to-list 'safe-local-eval-forms
             '(when (require 'hexcolor-mode nil t) (hexcolor-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; eval: (when (require 'hexcolor-mode nil t) (hexcolor-mode 1))
;; End:
;;; palory-theme.el ends here.
