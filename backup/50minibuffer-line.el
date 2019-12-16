;;; 50minibuffer-line.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Code:

(defun tl/minibuffer-line-pad (len)
  (propertize " "
              'display
              `((space :align-to
                       (- right
                          right-fringe
                          ,len)))))


(defun tl/minibuffer-line-main ()
  (let* ((rml `(:eval (if (and (boundp 'org-mode-line-string)
                               (> (length org-mode-line-string) 0))
                          (concat
                           (propertize "CLOCK:" 'face font-lock-comment-face)
                           org-mode-line-string)
                        "")))
         (rml-str (format-mode-line rml)) ; with the face of minibuffer-line?
         (extra-pad 1)
         (pad (tl/minibuffer-line-pad
               (+ (length rml-str) extra-pad))))
    (concat pad rml-str)))

(use-package minibuffer-line
  :init
  (progn
    (setq minibuffer-line-format
          '("" (:eval (tl/minibuffer-line-main)))))

  :config
  (progn
    (defadvice minibuffer-line--update (around left-concat-righ-align)
      (with-current-buffer minibuffer-line--buffer
        (erase-buffer)
        (insert (tl/minibuffer-line-main))))

    (ad-activate 'minibuffer-line--update)
    ;; (ad-deactivate 'minibuffer-line--update)
    (minibuffer-line-mode +1)))


(provide '50minibuffer-line)
;;; 50minibuffer-line.el ends here
