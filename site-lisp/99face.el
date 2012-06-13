;;; 50face.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;  M-x list-colors-display RET
;;  M-x list-faces-display RET

;;; useful macro
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




;; (defun set-face-color-1 (list)
;;   (let (face color)
;;     (while list
;;       (setq face (pop list)
;;             color (pop list))
;;       ;; (make-face face)
;;       (set-face-attribute face nil
;;                           :background nil
;;                           :box nil
;;                           :foreground (symbol-name color)
;;                           :inherit nil
;;                           :slant 'normal
;;                           :strike-through nil
;;                           :underline nil
;;                           ;; :height 1.0
;;                           :weight 'normal))))

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
;;;

;;; face
;; M-x list-faces-display
(defconst +f-color+ "#b6d3d6" "foreground color")
(defconst +b-color+ "#282c30" "background color")


(when (eq window-system 'ns)
  (setq initial-frame-alist `((tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (width . 268)
                              (height . 65)
                              (top . 22) ; (frame-parameter nil 'top)
                              (left . 0)
                              (alpha . (98 98)) ; first number is for the active window and the second for the inactive window
                              ;; (right-fringe)
                              ;; (left-fringe)
                              ;; (scroll-bar-width . 12)
                              (vertical-scroll-bars . right))

        ;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
        ;; use copy-sequence here -- it doesn't copy the list elements, just the
        ;; list's cons cells.  Use copy-alist instead.
        default-frame-alist (copy-alist initial-frame-alist))

  ;; (set-foreground-color +f-color+) ; "lightblue"
  ;; (set-background-color +b-color+) ; "#1D2C3F" "#102332" "#003c3c"
  (set-mouse-color "gray80")
  (set-cursor-color "#cd0000")

  (set-face-foreground 'font-lock-builtin-face "LightSteelBlue")
  (set-face-foreground 'font-lock-comment-face "#008888")
  (set-face-foreground 'font-lock-constant-face "Aquamarine")
  ;; (set-face-foreground 'font-lock-doc-string-face "DarkOrange")
  (set-face-foreground 'font-lock-function-name-face "LightCoral")
  (set-face-foreground 'font-lock-keyword-face "DarkOliveGreen2")
  (set-face-foreground 'font-lock-preprocessor-face "#79C96D")
  ;; (set-face-foreground 'font-lock-reference-face "SlateBlue")
  (set-face-foreground 'font-lock-string-face "#a800a8")
  (set-face-foreground 'font-lock-type-face "#4492e1")
  (set-face-foreground 'font-lock-variable-name-face "DodgerBlue2")
  (set-face-foreground 'font-lock-warning-face "Pink")

  (set-face-attribute 'highlight nil :background "LightBlue4" :foreground 'unspecified)
  (set-face-attribute 'link nil :background 'unspecified :foreground "#00cdcd" :underline t)

  ;;(set-face-background 'trailing-whitespace "#999999")

  ;;(set-face-foreground 'menu "#50bbff")
  ;;(set-face-background 'menu "gray20")


  (eval-after-load "org"
    '(progn
       (set-face-foreground 'org-level-1 "lightcoral")
       (set-face-foreground 'org-level-2 "dodgerblue2")
       (set-face-foreground 'org-level-3 "darkolivegreen2")
       (set-face-foreground 'org-level-4 "lightsteelblue")
       (set-face-foreground 'org-level-5 "cyan4")
       (set-face-foreground 'org-level-6 "Aquamarine")
       (set-face-foreground 'org-level-7 "LightSteelBlue")
       (set-face-foreground 'org-level-8 "LightSalmon")
       (set-face-foreground 'org-hide +b-color+)))




  (set-face-foreground 'icompletep-keys "LawnGreen")
  (set-face-foreground 'icompletep-determined "DarkMagenta")
  (set-face-foreground 'icompletep-choices "burlywood")

  ;; (set-face-background 'highlight-current-line-face "#222222")

  ;; background #282c30 ==> #484c50
  (set-face-attribute 'region nil :foreground 'unspecified :background "#484c50")
  (set-face-attribute 'secondary-selection nil :foreground 'unspecified :background "#485c60")

  ;; (set-face-foreground 'setnu-line-number-face "DarkSlateGray")


  (eval-after-load "whitespace"
    '(progn
       (set-face-attribute 'whitespace-space nil :background 'unspecified)))


  (eval-after-load "anything-config"
    '(progn
       (set-face-background 'anything-ff-directory "#222222")))



  (eval-after-load "ido"
    '(progn
       (set-face-attribute 'ido-subdir nil :foreground "DarkTurquoise" :weight 'bold)
       (set-face-attribute 'ido-first-match nil :foreground "DarkMagenta")))

  (eval-after-load "woman"
    '(progn
       (set-face-attribute 'woman-addition nil :foreground "DarkMagenta" :weight 'bold)
       (set-face-attribute 'woman-bold nil :foreground "cyan4" :weight 'bold)
       (set-face-attribute 'woman-italic nil :foreground "orange2" :underline nil)
       (set-face-attribute 'woman-unknown nil :foreground "LightSalmon2"))))



;;; font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-global-modes '(not text-mode)
      font-lock-verbose nil
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))




(setq tooltip-frame-parameters  '((name . "tooltip")
                                  (internal-border-width . 1)
                                  (border-width . 0)))


;;; tool bar
;;(tool-bar-mode nil)
;;(set-face-attribute 'tool-bar nil :background "DarkSlateGrey")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  cursor config
;; (setq-default cursor-type 'bar)
;;(setq x-stretch-cursor nil) ; *Non-nil means draw block cursor as wide as the glyph under it.

(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(setq blink-cursor-delay 0.2
      blink-cursor-interval 0.3)

(setq ring-bell-function 'ignore)
;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function
;;       (lambda ()
;;         (unless (memq this-command
;;                       '(isearch-abort abort-recursive-edit find-file
;;                         exit-minibuffer keyboard-quit))
;;           (ding))))

;; (require 'cursor-chg)
;; (change-cursor-mode 1) ; On for overwrite/read-only/input mode
;; (toggle-cursor-type-when-idle nil) ; On when idle

;;; Change cursor color according to mode
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "DodgerBlue"
           (if overwrite-mode "yellow"
             "#cd0000"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)


(defun underline-with-char (char)
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


(provide '99face)
