;;; 02utils.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defun pl/future-time-string (delay)
  (format-time-string "%H:%M"
                      (seconds-to-time (+ (time-to-seconds (current-time))
                                          delay))))



(provide '02utils)
;;; 02utils.el ends here
