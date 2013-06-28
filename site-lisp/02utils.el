;;; 02utils.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


;;; neat stuffs
(require 'key-chord nil t)

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


;;; face :: useful macro ----------------------------------------------------
(defmacro set-face-color (&rest list)
  `(set-face-color-1 (quote ,list)))

(defun set-face-color-1 (list)
  (let (face f-color b-color)
    (while list
      (setq face (pop list)
            f-color (pop list)
            b-color (pop list))
      ;; (make-face face)
      (set-face-attribute face nil
                          :background b-color
                          :box nil
                          :foreground f-color
                          :inherit nil
                          :slant 'normal
                          :strike-through nil
                          :underline nil
                          ;; :height 1.0
                          :weight 'normal))))

(defun get-face-hex (red green blue)
  "Return string hex of color specified by RED GREEN BLUE."
  (format "#%02x%02x%02x" (lsh red -8) (lsh green -8) (lsh blue -8)))

(defun get-face-step (list number color)
  "Return NUMBER of gradient for LIST of index COLOR."
  (let ((start (nth color (car list)))
        (end (nth color (cadr list))))
    (if (= end start)
        (make-list number start)
      (number-sequence start end (/ (- end start) (- number 1))))))

(defun get-face-gradient (face-prefix number color-start color-end)
  "Create NUMBER of FACE-PREFIX from COLOR-START to COLOR-END."
  (let* ((list (list (color-values color-start) (color-values color-end)))
         (red (get-face-step list number 0))
         (green (get-face-step list number 1))
         (blue (get-face-step list number 2))
         (num 0))
    (while (< num number)
      (let ((face (make-face
                   (intern
                    (concat face-prefix (number-to-string (1+ num)))))))
        (set-face-attribute face
                            nil :foreground
                            (get-face-hex (nth num red)
                                          (nth num green)
                                          (nth num blue))))
      (set 'num (1+ num)))))

;; --------------------------------------------------------------------------


(provide '02utils)
;;; 02utils.el ends here
