
;; Whether or not the buffer has been modified:
;; ** – modified since last save
;; -- – not modified since last save
;; %* – read-only, but modified
;; %% – read-only, not modifed


;; (copy-face 'default 'my-ml-position-face)
;; (set-face-foreground 'my-ml-position-face "Blue2")
;; (set-face-background 'my-ml-position-face nil)
;; (setf mode-line-position '(:eval (if (>= (current-column) 80)
;;                                      '(:propertize "(%l,%c)" face ml-position-face)
;;                                      '(:propertize "(%l,%c)" face ml-position-normal-face))))


;; (when window-system
;;   (let ((help-echo
;;          "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))
;;     (setq mode-line-position
;;           `((size-indication-mode
;;              (8 ,(propertize " of %I" 'help-echo help-echo)))
;;             (line-number-mode
;;              ((column-number-mode
;;                (10 ,(propertize " (%l,%c)" 'help-echo help-echo))
;;                (6 ,(propertize " L%l" 'help-echo help-echo))))
;;              ((column-number-mode
;;                (5 ,(propertize " C%c" 'help-echo help-echo)))))))))


(defface ml-position-normal-face '((((type x w32 mac))
                                 ;; #060525
                                 (:foreground "CornflowerBlue" :inherit bold))
                                (((type tty))
                                 (:foreground "blue")))
  "Face used to display the position in the mode line.")


(defface ml-position-face '((((type x w32 mac))
                                 ;; #060525
                                 (:foreground "orange red" :inherit bold))
                                (((type tty))
                                 (:foreground "blue")))
  "Face used to display the position of over 80 column in the mode line.")



(let* ((help-echo
        "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")
       (dashes (propertize "-" 'help-echo help-echo))
       (standard-mode-line-format
        (list
         "%e"
         (propertize "-" 'help-echo help-echo)
         'mode-line-mule-info
         'mode-line-client
         'mode-line-modified
         'mode-line-remote
         'mode-line-frame-identification
         'mode-line-buffer-identification
         (propertize "   " 'help-echo help-echo)
         "%p"
         ;; " of %I"
         `(:eval (if (>= (current-column) 80)
                     '(:propertize "(%l,%c)" face ml-position-face)
                     '(:propertize "(%l,%c)" face ml-position-normal-face)))
         `(:eval point-mode-line-string)
         '(vc-mode vc-mode)
         (propertize "  " 'help-echo help-echo)
         'mode-line-modes
         `(which-func-mode ("" which-func-format ,dashes))
         `(:eval (if (and mark-active (not (equal (mark) (point))))
                     (format "Over:%d lines,%d chars  "
                             (count-lines (region-beginning)
                                          (region-end))
                             (- (region-end) (region-beginning)))))
         `(global-mode-string ("" global-mode-string))
         (propertize "-%-" 'help-echo help-echo)))
       (standard-mode-line-modes
        (list
         (propertize "%[(" 'help-echo help-echo)
         `(:propertize ("" mode-name)
                       help-echo "mouse-1: major mode, mouse-2: major mode help, mouse-3: toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         ;; `(:propertize ("" minor-mode-alist)
         ;;               mouse-face mode-line-highlight
         ;;               help-echo "mouse-2: minor mode help, mouse-3: toggle minor modes"
         ;;               local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: widen"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-2 #'mode-line-widen))
         (propertize ")%] " 'help-echo help-echo))))

  (setq-default mode-line-format standard-mode-line-format)
  (setq-default mode-line-modes standard-mode-line-modes))


(setq global-mode-string
      '(""
        appt-mode-string
        working-mode-line-message
        display-time-string " "
        ;; battery-mode-line-string " "
        ;; win:mode-string
        "["
        (:propertize winring-name  face font-lock-constant-face)
        "]"
        emms-mode-line-string " "
        emms-mode-line-icon-function
        emms-lyrics-mode-line-string " "
        emms-playing-time-string))

(defvar point-mode-line-string nil)
(defun toggle-display-point ()
  (interactive)
  (if point-mode-line-string
      (setq point-mode-line-string nil)
      (setq point-mode-line-string '(:eval (format "P[%d] " (point))))))

(provide '50mode-line)