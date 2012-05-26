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

(deftheme palory
  "The palory theme.")

(let ((palory-fg "#b6d3d6")
      (palory-bg "#282c30"))

  (custom-theme-set-faces
   'palory

   `(default ((t (:background ,palory-bg :foreground ,palory-fg))))

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



   ;; rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#93e0e3"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#f0dfaf"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#94bff3"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#dca3a3"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#8fb28f"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#8cd0d3"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#dfaf8f"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#dc8cc3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#d0bf8f"))))
   '(rainbow-delimiters-depth-10-face ((t (:foreground "#9fc59f"))))
   '(rainbow-delimiters-depth-11-face ((t (:foreground "#94bff3"))))
   '(rainbow-delimiters-depth-12-face ((t (:foreground "#8c5353"))))))




(provide-theme 'palory)

;; Local Variables:
;; no-byte-compile: t
;; End:
