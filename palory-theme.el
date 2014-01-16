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

;;; base on https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el

;; the easy way to get following vlaue is: `customize face' => Apply and save => copy and paste

(deftheme palory
  "The palory theme.")

(let ((palory-fg "#b6d3d6")
      (palory-fg-1 "#656555")
      (palory-bg-1 "#2b2b2b")
      (palory-bg-05 "#383838")
      (palory-bg "#282c30")
      (palory-bg+1 "#4f4f4f")
      (palory-bg+2 "#5f5f5f")
      (palory-bg+3 "#6f6f6f")
      (palory-red+1 "#dca3a3")
      (palory-red "#cc9393")
      (palory-red-1 "#bc8383")
      (palory-red-2 "#ac7373")
      (palory-red-3 "#9c6363")
      (palory-red-4 "#8c5353")
      (palory-orange "#dfaf8f")
      (palory-yellow "#f0dfaf")
      (palory-yellow-1 "#e0cf9f")
      (palory-yellow-2 "#d0bf8f")
      (palory-green-1 "#5f7f5f")
      (palory-green "#7f9f7f")
      (palory-green+1 "#8fb28f")
      (palory-green+2 "#9fc59f")
      (palory-green+3 "#afd8af")
      (palory-green+4 "#bfebbf")
      (palory-cyan "#93e0e3")
      (palory-blue+1 "#94bff3")
      (palory-blue "#8cd0d3")
      (palory-blue-1 "#7cb8bb")
      (palory-blue-2 "#6ca0a3")
      (palory-blue-3 "#5c888b")
      (palory-blue-4 "#4c7073")
      (palory-blue-5 "#366060")
      (palory-magenta "#dc8cc3"))

  (custom-theme-set-faces
   'palory
   ;; basic coloring
   `(default ((t (:background ,palory-bg :foreground ,palory-fg))))
   '(cursor ((t (:foreground unspecified :background "#cd0000"))))

   ;; mode-line
   '(mode-line-buffer-id ((t (:foreground "#90377d"))))
   '(mode-line-emphasis ((t (:foreground "Magenta"))))
   '(mode-line ((t (:foreground "SteelBlue2" :background "#000000" :box nil))))
   '(mode-line-inactive ((t (:foreground "PaleTurquoise3" :background "#202020" :box nil)))) ; #222222
   '(mode-line-highlight ((t (:box nil))))

   ;; minibuffer
   '(minibuffer-prompt ((t (:foreground "#ff8c00" :weight bold))))

   ;; fringe
   `(fringe ((t (:foreground "green" :background "#232235"))))

   ;; region
   '(region ((t (:foreground unspecified :background "#484c50"))))

   ;; secondary-selection
   '(secondary-selection ((t (:foreground unspecified :background "#485c60"))))

   ;; tooltip
   '(tooltip ((t (:foreground "#111111" :background "#bcc8dd"))))

   ;; toolbar
   '(tool-bar ((t (:background "DarkSlateGrey"))))

   ;; highlight-symbol
   `(highlight-symbol-face ((t (:background "dodgerblue3" :foreground ,palory-fg))))

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
   `(org-hide ((t (:foreground ,palory-bg))))
   '(org-level-1 ((t (:foreground "lightcoral"))))
   '(org-level-2 ((t (:foreground "dodgerblue2"))))
   '(org-level-3 ((t (:foreground "darkolivegreen2"))))
   '(org-level-4 ((t (:foreground "lightsteelblue"))))
   '(org-level-5 ((t (:foreground "cyan4"))))
   '(org-level-6 ((t (:foreground "Aquamarine"))))
   '(org-level-7 ((t (:foreground "LightSteelBlue"))))
   '(org-level-8 ((t (:foreground "LightSalmon"))))
   '(org-habit-alert-face ((t (:foreground "#228822" :background "gold"))))



   ;; font lock
   '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
   '(font-lock-comment-face ((t (:foreground "#008888"))))
   '(font-lock-constant-face ((t (:foreground "#22ccee"))))
   '(font-lock-doc-face ((t (:foreground "turquoise4"))))
   '(font-lock-function-name-face ((t (:foreground "LightCoral"))))
   '(font-lock-keyword-face ((t (:foreground "DarkOliveGreen2"))))
   '(font-lock-preprocessor-face ((t (:foreground "#79C96D"))))
   '(font-lock-string-face ((t (:foreground "#a800a8"))))
   '(font-lock-type-face ((t (:foreground "#4492e1"))))
   '(font-lock-variable-name-face ((t (:foreground "DodgerBlue2"))))
   '(font-lock-warning-face ((t (:foreground "Pink"))))

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
   '(ace-jump-face-background ((t (:foreground "#007799"))))

   ;; cedet
   '(pulse-highlight-start-face ((t (:background "#222222"))))
   '(semantic-tag-boundary-face ((t (:overline "#227777"))))
   '(semantic-decoration-on-private-members-face ((t (:background "#333333"))))
   '(semantic-decoration-on-unparsed-includes ((t (:background "#555555"))))

   ;; volatile highlights
   '(vhl/default-face ((t (:background "#332244"))))

   ;; hl-line
   '(hl-line ((t (:background "#222222"))))

   ;; col-highlight
   '(col-highlight ((t (:background "#222222"))))

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
   '(shm-quarantine-face ((t (:background "#9e4f75"))))

   ;; linum
   '(linum ((t (:background "#102235" :foreground "#607b8b"))))

   ;; popup
   '(popup-tip-face ((t (:background "#a2cd5a" :foreground "#232323"))))

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
   `(ansi-color-names-vector ['palory-bg ,palory-red ,palory-green ,palory-yellow
                                         ,palory-blue ,palory-magenta ,palory-cyan ,palory-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,palory-bg-05)))




   ;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))



(provide-theme 'palory)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; palory-theme.el ends here.
