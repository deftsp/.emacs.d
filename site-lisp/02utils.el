;;; 02utils.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defun pl/future-time-string (delay)
  (format-time-string "%H:%M"
                      (seconds-to-time (+ (time-to-seconds (current-time))
                                          delay))))



(defun pl/apostrophe-key-chord ()
  (interactive)
  (let* ((regionp (region-active-p))
         (end (if regionp
                  (prog1 (region-end)
                    (goto-char (region-beginning)))
                0)))
    (insert "`")
    (save-excursion
      (if regionp
          (goto-char (+ end 1)))
      (insert "'"))))

(defun pl/underline-with-char (char)
  (interactive (list (read-from-minibuffer "Char: ")))
  (when (= 0 (length char))
    (error "Need a character"))
  (setq char (aref char 0))             ; Ignore everything but the first char.
  (save-excursion
    (goto-char (point-at-eol))
    (insert "\n"
            (make-string (- (point-at-eol)
                            (point-at-bol))
                         char))))


(provide '02utils)
;;; 02utils.el ends here
