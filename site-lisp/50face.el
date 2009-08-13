;;; tsp-face.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;;  M-x list-colors-display RET
;;  M-x list-faces-display RET

;;; useful macro

(defmacro tsp-face (&rest list)
  `(tsp-face-1 (quote ,list)))

(defun tsp-face-1 (list)
  (let (face color)
    (while list
      (setq face (pop list)
            color (pop list))
      (make-face face)
      (set-face-attribute face nil
                          :background "black"
                          :box nil
                          :foreground
                          (or (cdr (assoc color
                                         '((blue . "cyan")
                                           (normal . "white")
                                           (purple . "magenta")
                                           (underline . "white"))))
                             (symbol-name color))
                          :inherit nil
                          :slant 'normal
                          :strike-through nil
                          :underline (eq color 'underline)
                          ;; :height 1.0
                          :weight 'normal))))

(defun tsp-face-hex (red green blue)
  "Return string hex of color specified by RED GREEN BLUE."
  (format "#%02x%02x%02x" (lsh red -8) (lsh green -8) (lsh blue -8)))

(defun tsp-face-step (list number color)
  "Return NUMBER of gradient for LIST of index COLOR."
  (let ((start (nth color (car list)))
        (end (nth color (cadr list))))
    (if (= end start)
        (make-list number start)
        (number-sequence start end (/ (- end start) (- number 1))))))

(defun tsp-face-gradient (face-prefix number color-start color-end)
  "Create NUMBER of FACE-PREFIX from COLOR-START to COLOR-END."
  (let* ((list (list (color-values color-start) (color-values color-end)))
         (red (tsp-face-step list number 0))
         (green (tsp-face-step list number 1))
         (blue (tsp-face-step list number 2))
         (num 0))
    (while (< num number)
      (let ((face (make-face
                   (intern
                    (concat face-prefix (number-to-string (1+ num)))))))
        (set-face-attribute face
                            nil :foreground
                            (tsp-face-hex (nth num red)
                                          (nth num green)
                                          (nth num blue))))
      (set 'num (1+ num)))))
;;;

;; (tsp-face info-node blue
;;           info-title-1 yellow
;;           info-title-2 green1
;;           info-title-3 green2
;;           info-title-4 green3
;;           info-menu-header LawnGreen
;;           info-menu-5 normal
;;           info-xref blue)



;;(set-cursor-color "#cd0000")

;;(set-mouse-color "gray80")
;; 设置背景颜色和字体颜色
;;(set-foreground-color "lightblue")
;; (set-background-color "#003c3c")
;; (set-background-color "#1D2C3F")
;;(set-background-color "#102332")
;;(if (eq window-system 'x)
;;(if window-system
;;    (progn
;;      (set-foreground-color "lightblue")
;;      (set-background-color "#102332"))
;;(set-background-color "red")
;;)

;;; font lock
(global-font-lock-mode t)

(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not text-mode))
(setq font-lock-verbose nil)
(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))
;; (setq font-lock-maximum-size (* 70 1024))
;;(setq font-lock-support-mode 'lazy-lock-mode) ; default jit-lock-mode

;;注释字体斜体
;; (setq font-lock-comment-face 'bold-italic)
;; (setq font-lock-comment-face 'italic)

;; see m-x list-faces-display too.................................................

;; Adjust the colors to preference:
;; (set-face-foreground 'font-lock-type-face "forestgreen")
;; (set-face-foreground 'font-lock-comment-face "#9FB3C4");;cadetblue
;; (set-face-foreground 'font-lock-function-name-face "#E5E07A")
;; (set-face-foreground 'font-lock-keyword-face "darkcyan")
;; (set-face-foreground 'font-lock-string-face "#A800A8")
;; (set-face-foreground 'font-lock-variable-name-face "Coral")
;; (set-face-foreground 'font-lock-warning-face "yellow")
;; (set-face-foreground 'font-lock-constant-face "#93953E")
;;include的颜色
;;(set-face-foreground 'font-lock-builtin-face "Magenta")
;;边缘颜色设置
;;(set-face-foreground 'fringe "Yellow")
;;(set-face-background 'fringe "#000000")
;;(set-face-foreground 'menu "#50bbff")
;;(set-face-background 'menu "gray20")

;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
;; (set-face-foreground 'highlight "#50bbff")
;; (set-face-background 'highlight "gray40")

;; (set-face-foreground 'region "white")
;; (set-face-background 'region "#136eed")
;; (set-face-foreground 'secondary-selection "skyblue")
;; (set-face-background 'secondary-selection "darkblue")

;;mode-line
;;(set-face-foreground 'mode-line "blue4")
;;(set-face-background 'mode-line "#808080")
;;(set-face-foreground 'mode-line-buffer-id "#90377d")
;;(set-face-attribute 'mode-line nil :font "Tahoma")
;;(set-face-attribute 'mode-line nil :height 0.9)
;;(set-face-attribute 'mode-line nil :box ":style nil")
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-foreground 'mode-line-inactive "black")
;;(set-face-background 'mode-line-inactive "#808080")
;;(set-face-attribute 'mode-line-inactive nil :box ":style nil")

;;(set-face-foreground 'mode-line-highlight "#008c84")
;;(set-face-background 'mode-line-highlight "#303030")
;;(set-face-attribute 'mode-libne-highlight nil :box ":line-width 0 :style nil")


;;(set-face-foreground 'which-func  "#912033")
;;(set-face-background 'trailing-whitespace "#999999")

(when window-system
  ;;   (let (mode-line-in-global-map cdr-mode-line-in-global-map)
  ;;     (if (setq mode-line-in-global-map (assoc 'mode-line global-map))
  ;;         (if (setq cdr-mode-line-in-global-map (cdr mode-line-in-global-map))
  ;;             (setcdr cdr-mode-line-in-global-map nil))))
  ;; (setq initial-frame-alist default-frame-alist)
  (setq default-frame-alist '((foreground-color . "#b6d3d6")
                              ;; (background-color . "#000000")
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              ;; (width . 126)
                              ;; (height . 40)
                              ;; (right-fringe)
                              ;; (left-fringe)
                              ;; (scroll-bar-width . 12)
                              (vertical-scroll-bars . right))))

;; (defun my-new-frame-setting ()
;;   (set-frame-font "Bitstream Vera Sans Mono-10")
;;   (let ((fontset (frame-parameter nil 'font)))
;;     (dolist
;;         (charset '(han symbol cjk-misc bopomofo))
;;       (set-fontset-font fontset charset '("Microsoft Yahei" . "unicode-bmp")))))

;; (add-to-list 'after-make-frame-functions
;;              (lambda (new-frame)
;;                (select-frame new-frame)
;;                (if window-system
;;                  (my-new-frame-setting))))




;; (defun billc-use-underline-p (&optional display)
;;   "Non-nil if DISPLAY supports underlining.
;; This is only sometimes accurate."
;;   (if (fboundp 'display-supports-face-attributes-p)
;;       (display-supports-face-attributes-p '(:underline) display)
;;     window-system))

;; (setq tooltip-frame-parameters  '((name . "tooltip")
;;                                   (internal-border-width . 1)
;;                                   (border-width . 0)))


;;; tool bar
;;(tool-bar-mode nil)
;;(set-face-attribute 'tool-bar nil :background "DarkSlateGrey")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  cursor config
;;将光标设置成短线，而不是方块
;;(setq-default cursor-type 'bar)
;; 如果设置为 t，光标在 TAB 字符上会显示为一个大方块 :)。
;;(setq x-stretch-cursor nil)

;;use bar cursor for all frames
;; (setq initial-frame-alist
;;       (cons '(cursor-type . bar)
;;             (copy-alist initial-frame-alist)))
;; (setq default-frame-alist
;;       (cons '(cursor-type . bar)
;;             (copy-alist default-frame-alist)))


;;指针不要闪，我得眼睛花了
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; (setq blink-cursor-delay 0.2)
;; (setq blink-cursor-interval 0.3)
;;在console 下 , 我也不想看到屏幕不停的闪
;; (setq ring-bell-function (lambda ()  t))

;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function
;;       (lambda ()
;;         (unless (memq this-command
;;                       '(isearch-abort abort-recursive-edit find-file
;;                         exit-minibuffer keyboard-quit))
;;           (ding))))

;; Changing Cursor Dynamically
(defun tsp-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  (set-cursor-color (if buffer-read-only "DodgerBlue"
                        (if overwrite-mode
                            "yellow"
                            "#cd0000"))))
(add-hook 'post-command-hook 'tsp-set-cursor-color-according-to-mode)


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


