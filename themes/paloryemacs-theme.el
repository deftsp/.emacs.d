;;; paloryemacs-theme.el ---

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

;;; Commentary: paloryemacs theme, for programmer

;;; Code:

;;; base on zenburn and solarized
;; https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el and
;; https://github.com/sellout/emacs-color-theme-solarized
;; https://github.com/bbatsov/solarized-emacs
;; https://github.com/purcell/color-theme-sanityinc-solarized

;; the easy way to get following vlaue is: `customize face' => Apply and save => copy and paste
;; Likes "#032238"

(deftheme paloryemacs "The Paloryemacs color theme. Base on zenburn and solarized")

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
      (green   "#859900")
      (inactive1 "#43586d")
      (inactive2 "#33485d"))

  (custom-theme-set-faces
   'paloryemacs
   ;; basic coloring
   `(default ((t (:background ,base03 :foreground ,base0))))
   '(cursor ((t (:foreground unspecified :background "#cd0000"))))

   ;; mode-line
   '(mode-line-buffer-id ((t (:foreground "#90377d"))))
   '(mode-line-emphasis ((t (:foreground "Magenta"))))
   `(mode-line ((t (:foreground ,base1 :background ,base02 :box nil :height 1.0 :family "mplus Nerd Font"))))
   `(mode-line-inactive ((t (:inherit 'mode-line :foreground ,base0  :background ,base03))))
   `(mode-line-highlight ((t (:inherit 'mode-line :background ,base01 :foreground ,base2))))

   '(minibuffer-prompt ((t (:foreground "chartreuse2" :weight bold))))
   `(fringe ((t (:foreground "#00cbcb" :background ,base02))))
   `(region ((t (:foreground unspecified :background "#244252"))))
   '(secondary-selection ((t (:foreground unspecified :background "#485c60"))))

   ;; minibuffer-line
   `(minibuffer-line ((t (:inherit 'mode-line-inactive :foreground ,base0 :background ,base03))))


   ;; the split line of two window
   `(vertical-border ((t (:foreground ,base02))))

   `(variable-pitch ((t (:family "Roboto Condensed"))))

   ;; powerline
   `(powerline-active1 ((t (:background "#6b6b6b" :foreground "#fefefe" :inherit mode-line))))
   `(powerline-active2 ((t (:background "#182e3f" :foreground "#adadad" :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,inactive1 :foreground "#bbbbbb" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,inactive2 :foreground "#9b9b9b" :inherit mode-line-inactive))))
   `(powerline-evil-insert-face ((t (:foreground ,inactive2 :background "OliveDrab1" :weight bold :inherit mode-line))))
   `(powerline-evil-normal-face ((t (:foreground ,inactive2 :background "orchid" :weight bold :inherit mode-line))))
   `(powerline-evil-visual-face ((t (:foreground ,inactive2 :background "Purple" :weight bold :inherit mode-line))))
   `(powerline-evil-motion-face ((t (:foreground ,inactive2 :background "Cyan" :weight bold :inherit mode-line))))
   `(powerline-evil-emacs-face ((t (:foreground ,inactive2 :background "#7cfa42" :weight bold :inherit mode-line))))
   `(powerline-evil-replace-face ((t (:foreground ,inactive2 :background "orchid" :weight bold :inherit mode-line))))
   `(powerline-evil-operator-face ((t (:foreground ,inactive2 :background "maroon" :weight bold :inherit mode-line))))
   `(powerline-evil-lisp-face ((t (:background "#adff2f" :foreground "blue" :inherit mode-line))))
   `(powerline-evil-lispy-face ((t (:background "orange" :foreground "white" :inherit mode-line))))
   `(powerline-file-base-info-face ((t (:background ,base00 :foreground "#fefefe" :inherit mode-line))))

   `(powerline-buffer-id-face ((t (:background ,base02 :foreground "#fefefe" :inherit mode-line))))
   `(powerline-vc-face ((t (:background ,base01 :foreground "#f4bb66" :inherit mode-line))))
   `(powerline-workgroups-face ((t (:background "#778899" :foreground "#0a3540" :inherit mode-line))))
   `(powerline-ace-window-path-face ((t (:background "#174652" :foreground "#0a3540" :inherit mode-line))))
   `(powerline-winum-number-face ((t (:background "#174652" :foreground "#0a3540" :inherit mode-line))))
   `(powerline-mode-line-modified-face ((t (:foreground "#ee1280" :background unspecified))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:background "orchid" :foreground unspecified))))

   ;; vc
   `(vc-state-base-face ((t (:background unspecified :foreground ,base0))))

   ;; tooltip
   '(tooltip ((t (:foreground "#111111" :background "#bcc8dd"))))

   ;; toolbar
   '(tool-bar ((t (:background "DarkSlateGrey"))))

   `(header-line ((t (:foreground ,base2 :background ,base02))))

   ;; highlight-symbol
   `(highlight-symbol-face ((t (:background "dodgerblue3" :foreground ,base0))))

   ;; highlight
   '(highlight ((t (:foreground unspecified :foreground "#66e2e2" :background "#454545")))) ; use `unspecified' or `nil' both ok

   ;; link
   '(link ((t (:foreground "#33b2ef" :background unspecified :underline t :weight bold))))
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
   '(show-paren-match ((t (:foreground "#222222" :background "#3cdeef"))))
   '(show-paren-mismatch ((t (:foreground "white" :background "#dd2222"))))

   ;; mic-paren
   '(paren-face-match ((t (:foreground "#222222" :background "DeepSkyBlue3"))))

   ;; which-fun
   '(which-func ((t (:foreground "Yellow"))))

   ;; calendar
   '(holiday ((t (:background "#d01ce2" :foreground "#d2d2d2"))))
   '(diary ((t (:background "cyan2" :foreground "#222222"))))
   '(calendar-today ((t (:background "LightGoldenrod" :foreground "#000064"))))

   ;; term
   '(term-color-blue ((t (:foreground "DeepSkyBlue4"))))

   ;;; mmm-mode
   '(mmm-default-submode-face ((t (:background "gray97"))))

   ;; adoc-mode
   '(markup-meta-face ((t (:height 120 :foreground "#999999"))))
   '(markup-meta-hide-face ((t (:height 0.9 :foreground "#777777"))))
   '(adoc-table-del ((t (:background "MidnightBlue" :foreground "#eeeeee"))))

   ;; org-mode
   '(org-headline-done ((t (:strike-through t))))
   '(org-done ((t (:strike-through t))))
   `(org-hide ((t (:foreground ,base03))))
   '(org-level-1 ((t (:inherit outline-1   :foreground "#1e90ff" :weight normal :height 1.0))))
   '(org-level-2 ((t (:inherit org-level-1 :foreground "#7ad224" :height 1.0))))
   '(org-level-3 ((t (:inherit org-level-1 :foreground "#dfaf8f"))))
   '(org-level-4 ((t (:inherit org-level-1 :foreground "#8cd0d3"))))
   '(org-level-5 ((t (:inherit org-level-1 :foreground "#8fb28f"))))
   '(org-level-6 ((t (:inherit org-level-1 :foreground "#dca3a3"))))
   '(org-level-7 ((t (:inherit org-level-1 :foreground "#94bff3"))))
   '(org-level-8 ((t (:inherit org-level-1 :foreground "#f0dfaf"))))
   '(org-level-9 ((t (:inherit org-level-1 :foreground "#93e0e3"))))
   '(org-date ((t (:foreground "#00bfee"))))
   '(org-link ((t (:inherit link :foreground "#e2d282" :underline t :weight normal))))
   '(org-habit-alert-face ((t (:foreground "#228822" :background "gold"))))
   `(org-agenda-structure ((t (:foreground "#26bbf8" :background ,base02 :weight bold))))
   '(org-scheduled ((t (:foreground "#8abeb2"))))
   '(org-scheduled-previously ((t (:foreground "#ae799f"))))
   '(org-scheduled-today ((t (:foreground "#46bbd2"))))
   '(org-agenda-calendar-sexp ((t (:foreground "MistyRose3"))))
   '(org-agenda-date ((t (:foreground "#36abf2" :weight bold))))
   '(org-agenda-date-today ((t (:foreground "#e8d882" :weight bold))))
   '(org-agenda-dimmed-todo-face ((t (:foreground "#778888" :weight bold))))
   '(org-warning ((t (:foreground "#ff6347" :weight bold))))
   '(org-drill-hidden-cloze-face ((t (:foreground "#eeeeee" :background "violet red"))))
   '(org-block ((t (:background "#20323e"))))
   '(org-block-begin-line ((t (:background "#30424e"))))
   '(org-block-end-line ((t (:background "#30424e"))))
   '(org-ellipsis ((t (:underline nil :foreground "#c5e666" :background unspecified))))
   '(org-mode-line-clock ((t (:background nil :foreground "#268bd2"))))

   '(warning ((t (:foreground "VioletRed" :weight bold))))

   ;; whitespace
   `(whitespace-space ((t (:foreground "#ff6622" :background ,base02))))


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

   ;; flx
   `(flx-highlight-face ((t (:foreground "#f1ed22" :background ,base02 :weight bold :underline nil))))

   ;; anything
   '(anything-ff-directory ((t (:background "#222222"))))

   ;; helm
   `(helm-selection ((t (:background ,base01 :foreground unspecified))))
   '(helm-visible-mark ((t (:background "turquoise4" :foreground unspecified))))

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

   ;; minimap
   `(minimap-active-region-background ((t (:background ,base02))))

   ;; bookmark+
   '(bmkp-light-fringe-autonamed ((t (:background "#778899" :foreground "#222222"))))
   '(bmkp-light-non-fringe-autonamed ((t (:background "DarkOrange1" :foreground "Black"))))

   ;; icicle
   '(icicle-current-candidate-highlight ((t (:background "orchid4"))))
   '(icicle-mustmatch-completion ((t (:box nil))))
   '(icicle-multi-command-completion ((t (:background unspecified :foreground "sandybrown"))))

   ;; dried & diredp
   '(dired-marked ((t (:background "#555555" :foreground "Pink"))))

   '(diredp-dir-heading ((t (:background "#555555" :foreground "magenta"))))
   '(diredp-file-name ((t (:foreground "dodger blue"))))
   '(diredp-dir-name ((t (:foreground "#222222" :background "burlywood"))))
   '(diredp-dir-priv ((t (:foreground "steel blue" :background unspecified :weight bold))))
   '(diredp-no-priv ((t (:foreground unspecified :background unspecified))))
   '(diredp-read-priv ((t (:foreground "deep sky blue" :background unspecified))))
   '(diredp-write-priv ((t (:foreground "yellow" :background unspecified))))
   '(diredp-exec-priv ((t (:foreground "red" :background unspecified))))
   '(diredp-flag-mark-line ((t (:foreground "white" :background "blue4"))))

   ;; irfc
   '(irfc-head-name-face ((t (:foreground "violet" :underline nil))))
   '(irfc-head-number-face ((t (:foreground "violet"))))
   '(irfc-reference-face ((t (:foreground "burlywood"))))


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

   ;; ace-window-mode-line
   '(aw-leading-char-face ((t (:foreground "red" :height 9.0))))
   '(aw-mode-line-face ((t (:foreground "violet" :background unspecified))))

   ;; winum
   '(winum-face ((t (:foreground "violet" :background unspecified))))

   ;; company-mode
   `(company-tooltip ((t (:background ,base01 :foreground "#dfdfdf"))))
   `(company-tooltip-common ((t (:background ,base1 :foreground "#aa3333"))))
   ;; '(company-tooltip-common-selection ((t (:background "#73c5bb" :foreground "White"))))
   `(company-tooltip-common-selection ((t (:background ,base1 :foreground "#ccff66"))))
   `(company-tooltip-selection ((t (:background ,base1 :foreground "#ccff66"))))
   `(company-scrollbar-bg ((t (:background ,base0))))
   `(company-scrollbar-fg ((t (:background ,base2))))
   '(company-tooltip-search ((t (:background "#aa3333" :foreground "#eeeeee"))))


   ;; cedet
   '(pulse-highlight-start-face ((t (:background "#222222"))))
   '(semantic-tag-boundary-face ((t (:overline "#227777"))))
   '(semantic-decoration-on-private-members-face ((t (:background "#333333"))))
   '(semantic-decoration-on-unparsed-includes ((t (:background "#555555"))))

   ;; ecb
   '(ecb-default-highlight-face ((t (:foreground "#666666"))))
   '(ecb-tag-header-face ((t (:foreground "gray10" :background "rosy brown"))))

   ;; emms
   '(emms-playlist-selected-face ((t (:foreground "magenta"))))
   '(emms-playlist-track-face ((t (:foreground "SteelBlue2"))))

   ;; elfeed
   '(elfeed-search-title-face ((t (:foreground "#dddddd"))))
   '(elfeed-search-unread-title-face ((t (:weight normal))))

   ;; volatile highlights
   '(vhl/default-face ((t (:background "#332244"))))

   ;; hl-line
   `(hl-line ((t (:background ,base02))))

   ;;ivy
   '(ivy-action ((t (:foreground "PaleGoldenrod"))))
   '(ivy-virtual ((t (:foreground "#536393"))))
   `(ivy-current-match ((t (:foreground "#dddddd" :background ,magenta :weight bold))))

   ;; col-highlight
   `(col-highlight ((t (:background ,base02))))

   ;; highlight-indentation
   '(highlight-indentation-face ((t (:background "#003b46"))))
   `(highlight-indentation-current-column-face ((t (:background "#196873"))))

   ;; comint
   '(comint-highlight-prompt ((t (:foreground "#eedd82" :weight bold))))

   ;; flymake
   '(flymake-warnline ((t (:background "#333300" :foreground "#ccccff"))))
   '(flymake-errline ((t (:background "#402222" :foreground "#cceecc"))))

   ;; git-gutter-fringe
   '(git-gutter-fr:modified ((t (:foreground "Magenta"))))
   '(git-gutter-fr:added ((t (:foreground "cyan"))))
   '(git-gutter-fr:deleted ((t (:foreground "white"))))

   ;; diff-hl
   `(diff-hl-insert ((t (:foreground ,green :background ,green))))
   `(diff-hl-change ((t (:foreground ,blue :background ,blue))))
   `(diff-hl-delete ((t (:foreground ,yellow :background ,yellow))))
   `(diff-hl-unknown ((t (:foreground)) ,violet :background ,violet))

   ;; eshell-prompt-extras
   `(paloryemacs/eshell-base-face ((t (:foreground ,base0 :background nil :font "mplus Nerd Font" :size 14))))
   `(epe-symbol-face ((t (:foreground "#d01ce2" :inherit paloryemacs/eshell-base-face))))
   `(epe-user-face ((t (:foreground "#648fc3" :inherit paloryemacs/eshell-base-face))))
   `(epe-host-face ((t (:foreground "#648fc3" :inherit paloryemacs/eshell-base-face))))
   `(epe-time-face ((t (:foreground "#e2d282" :inherit paloryemacs/eshell-base-face))))
   `(epe-dir-face ((t (:foreground "#94bff3" :inherit paloryemacs/eshell-base-face))))
   `(epe-venv-face ((t (:foreground "#dca3a3" :inherit paloryemacs/eshell-base-face))))
   `(epe-git-face ((t (:foreground "#22ccee" :inherit paloryemacs/eshell-base-face))))
   `(epe-delimiter-face ((t (:foreground ,base1 :inherit paloryemacs/eshell-base-face))))

   ;; structured-haskell-mode
   '(shm-current-face ((t (:background "#222222"))))
   '(shm-quarantine-face ((t (:background "#262230"))))

   ;; linum
   '(linum ((t (:background "#102530" :foreground "#607b8b"))))

   ;; native line numbers (emacs 26)
   `(line-number ((t (:foreground "#607b8b" :background "#04303d"))))
   `(line-number-current-line ((t (:foreground "#202050" :background "#657b83"))))

   ;; guide-key
   '(guide-key/key-face ((t (:foreground "violet"))))
   '(guide-key/highlight-command-face ((t (:foreground "#f9f69d"))))

   ;; gambit
   '(gambit-highlight-face ((t (:foreground "#000000"))))

   ;; hydra
   '(hydra-face-blue ((t (:foreground "#00bfee"))))

   ;; quack
   '(quack-pltish-paren-face ((t (:foreground "#ccffcc" :weight normal))))
   '(quack-pltish-comment-face ((t (:foreground "#008888" :weight normal))))
   '(quack-pltish-keyword-face ((t (:foreground "#bbbb99" :weight bold))))
   '(quack-pltish-selfeval-face ((t (:foreground "#a800a8"))))
   '(quack-pltish-defn-face ((t (:foreground "#ff7f00"))))
   '(quack-threesemi-semi-face ((t (:background unspecified))))
   '(quack-threesemi-text-face ((t (:background unspecified))))


   ;; narrow-indirect
   `(ni-mode-line-buffer-id ((t (:foreground "RosyBrown" :box nil))))

   ;; rainbow-delimiters
   '(rainbow-delimiters-unmatched-face ((t (:foreground unspecified :inherit show-paren-mismatch :strike-through t))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "#93e0e3" :weight bold))))
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
   'paloryemacs
   `(ansi-color-names-vector ['bg ,red ,green ,yellow ,blue ,magenta ,cyan ,base0])
   ;; fill-column-indicator
   `(fci-rule-color ,base01)))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))



(provide-theme 'paloryemacs)

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
;; eval: (auto-fill-mode -1)
;; End:
;;; paloryemacs-theme.el ends here.
