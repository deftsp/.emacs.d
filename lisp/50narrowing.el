;;; 50narrowing.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Code:

;;; Narrowing
;; Undo these with C-x n w
(put 'narrow-to-region 'disabled nil)     ; C-x n n
(put 'narrow-to-page   'disabled nil)     ; C-x n p
(put 'narrow-to-defun  'disabled nil)     ; C-x n d

;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
(global-set-key(kbd "C-x n i") 'pl/narrow-to-region-indirect)
(defun pl/narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let* ((buf-name (generate-new-buffer-name
                    (concat (buffer-name) "/INDIRECT["
                            (number-to-string start) ", "
                            (number-to-string end) "]")))
         (Buf (clone-indirect-buffer buf-name t)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (goto-char (point-min))
      (switch-to-buffer buf))))

(when (boundp 'ido-ignore-buffers)
  (add-to-list 'ido-ignore-buffers "^.*/INDIRECT\\[.*\\]$"))

;; (eval-after-load "evil"
;;   '(progn
;;      ;; (define-key evil-normal-state-map "m" 'evil-narrow-indirect)
;;      ;; (define-key evil-visual-state-map "m" 'evil-narrow-indirect)

;;      (evil-define-operator evil-narrow-indirect (beg end type)
;;        "Indirectly narrow the region from BEG to END."
;;        (interactive "<R>")
;;        (evil-normal-state)
;;        (pl/narrow-to-region-indirect beg end))))

;;; narrow-indirect.el --- Narrow using an indirect buffer that is a clone
;; http://www.emacswiki.org/emacs/NarrowIndirect
(eval-after-load "narrow-indirect"
  '(progn
     (setq ni-buf-name-prefix "IB%")
     (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-other-window)
     (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-other-window)
     (define-key ctl-x-4-map "np" 'ni-narrow-to-page-other-window)))


(provide '50narrowing)
;;; 50narrowing.el ends here
