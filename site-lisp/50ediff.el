;;; 50ediff.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; reuse the selected frame
(if window-system
 (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ; 'ediff-setup-windows-multiframe
(setq ediff-split-window-function 'split-window-horizontally) ; 'split-window-vertically
;;; 50ediff.el ends here
