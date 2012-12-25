
;; Whether or not the buffer has been modified:
;; ** – modified since last save
;; -- – not modified since last save
;; %* – read-only, but modified
;; %% – read-only, not modifed



(defface mode-line-position-normal-face
  '((((type x w32 ns))
     (:foreground "CornflowerBlue" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the position in the mode line.")


(defface mode-line-position-exceed-face
  '((((type x w32 ns))
     (:foreground "orange red" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the position of over 80 column in the mode line.")



;;;
(setq-default mode-line-position
              `((:eval point-mode-line-string)
                "%p"
                (:eval (if (and mark-active (not (equal (mark) (point))))
                           (format " Over:%d lines#%d chars  "
                                   (count-lines (region-beginning)
                                                (region-end))
                                   (- (region-end) (region-beginning)))))

                (line-number-mode (" " (:eval
                                        (propertize
                                         (format "◁%%l/%d " (count-lines (point-min) (point-max)))
                                         'face 'mode-line-position-normal-face))))
                (column-number-mode ("" (:eval
                                         (propertize
                                          (format "%%c/%d▷ " (pl/get-line-columns))
                                          'face (if (>= (current-column) 81)
                                                    'mode-line-position-exceed-face
                                                  'mode-line-position-normal-face)))
                                     ""))))


(let* ((help-echo
        "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\n mouse-3: Remove current window from display")
       (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
       (standard-mode-line-modes
        (list
         " "
         (propertize "%[" 'help-echo recursive-edit-help-echo)
         (propertize "(" 'help-echo help-echo)
         `(:propertize ("" mode-name)
                       help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\n mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode\nmouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 #'mode-line-widen))
         (propertize ")" 'help-echo help-echo)
         (propertize "%]" 'help-echo recursive-edit-help-echo))))
  (setq-default mode-line-modes standard-mode-line-modes)
  (setq-default mode-line-format
                `("%e"  ;  When Emacs is nearly out of memory for Lisp objects, a brief message saying so.  Otherwise, this is empty.
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  ,(propertize "    " 'help-echo help-echo)
                  mode-line-position
                  (vc-mode vc-mode)
                  " " mode-line-modes
                  (which-func-mode ("--" which-func-format "--"))
                  (global-mode-string ("" global-mode-string))
                  (working-mode-line-message (" " working-mode-line-message))
                  ,(propertize "-%-" 'help-echo help-echo))))


(setq global-mode-string
      '(" "
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

(defvar point-mode-line-string nil "point string to display on mode line")
(defun toggle-display-point ()
  (interactive)
  (if point-mode-line-string
      (setq point-mode-line-string nil)
    (setq point-mode-line-string '(:eval (format "P[%d] " (point))))))


(defun pl/get-line-columns ()
  (- (line-end-position) (line-beginning-position)))

(provide '50mode-line)
