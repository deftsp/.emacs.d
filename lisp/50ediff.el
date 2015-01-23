;;; 50ediff.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; reuse the selected frame
(when window-system
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ; 'ediff-setup-windows-multiframe
;; Note that you can also split the window depending on the frame width:
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))

;; (setq ediff-diff-options "-w") ; ignore white space

(defun pl/ediff-mode-init ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "J" 'ediff-next-difference) ; default to "j"
  (define-key ediff-mode-map "j" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'pl/ediff-mode-init)

;; Restoring the windows after Ediff quits
(defun pl/winner-undo-maybe ()
  (when winner-mode
    (winner-undo)))

(add-hook 'ediff-after-quit-hook-internal 'pl/winner-undo-maybe)


(provide '50ediff)
;;; 50ediff.el ends here
