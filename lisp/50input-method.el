;;; 50input-method.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(defun tl/on-select-previous-input-source (data)
  "Swith to insert when switch to Rime input method"
  (let ((source-id (plist-get data :source-id)))
    (when (string-equal source-id "im.rime.inputmethod.Squirrel.Rime")
      (with-selected-window (selected-window)
        (let ((state (bound-and-true-p evil-state)))
          (when (and state
                     (eq state 'normal)
                     (not (minibufferp))
                     (not isearch-mode))
            (call-interactively 'evil-insert-state)))))))

(provide '50input-method)
