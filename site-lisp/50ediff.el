;;; 50ediff.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; reuse the selected frame
(if window-system
 (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ; 'ediff-setup-windows-multiframe
;; Note that you can also split the window depending on the frame width:
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                        (split-window-vertically arg))))


(provide '50ediff)
;;; 50ediff.el ends here
