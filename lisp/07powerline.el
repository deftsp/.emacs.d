;;; 07powerline.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
;; installed patched fonts from https://github.com/Lokaltog/powerline-fonts .

;; vc.*face inherit the face of mode-line, it cause powerline-vc can not change
;; it's background and foreground. Defining vc-state-base-face not to inherit
;; will slove that.

(use-package powerline
  :init
  ;; M-x powerline-reset, after changing
  (setq powerline-text-scale-factor 0.8
        ;; powerline-default-separator 'utf-8 ; 'arrow
        powerline-image-apple-rgb nil
        ns-use-srgb-colorspace t)

  (let ((height (cl-case system-type
                  (darwin 20)
                  (gnu/linux 40)
                  (t 20))))
    ;; NOTE: the height of xpm image will be affected by `powerline-height', it might be have some problem in HiDPI
    ;; (Linux).
    (setq powerline-height height)))

(defvar powerline-git-state-mark-modeline t
  "When t git state mark on will work with powrline instead of in the front of
the modeline")

(defface powerline-workgroups-face
  '((t (:background "#778899" :foreground "#0a3540")))
  "Powerline workgroups face."
  :group 'powerline)

(defface powerline-ace-window-path-face
  '((t (:background "#174652" :foreground "#0a3540")))
  "powerline ace window path face."
  :group 'powerline)

(defface powerline-winum-number-face
  '((t (:background "#174652" :foreground "#0a3540")))
  "powerline winum number face."
  :group 'powerline)

(defface powerline-vc-face
  '((t (:background "#849c10" :foreground "#0a3540")))
  "Powerline vc face."
  :group 'powerline)

(defface powerline-anzu-face
  '((t (:background "#dfaf8f" :foreground "#5e747a")))
  "Powerline anzu face."
  :group 'powerline)

(defface powerline-file-base-info-face
  '((t (:background "#3d89d0" :foreground "#0a3540")))
  "Powerline file base info face."
  :group 'powerline)

(defface powerline-buffer-id-face
  '((t (:background "#008b8b" :foreground "#22eeee")))
  "Powerline buffer id face."
  :group 'powerline)

(defface powerline-mode-line-normal-face
  '((t (:foreground unspecified :background unspecified)))
  "Powerline mode line normal face."
  :group 'powerline)


(defface powerline-mode-line-modified-face
  '((t (:foreground "#cc2200")))
  "Powerline mode line modified face."
  :group 'powerline)



(defun strip-text-properties(text)
  (set-text-properties 0 (length text) nil text)
  text)

;; base on mode-line-modified
(defpowerline tl/mode-line-modified
  (list (propertize
         "%1*"
         'help-echo 'mode-line-read-only-help-echo
         'local-map (purecopy (make-mode-line-mouse-map
                               'mouse-1
                               #'mode-line-toggle-read-only))
         'face (if (buffer-modified-p)
                   'powerline-mode-line-modified-face
                 'powerline-mode-line-normal-face)
         'mouse-face 'mode-line-highlight)
        (propertize
         "%1+"
         'help-echo 'mode-line-modified-help-echo
         'local-map (purecopy (make-mode-line-mouse-map
                               'mouse-1 #'mode-line-toggle-modified))
         'face (if (buffer-modified-p)
                   'powerline-mode-line-modified-face
                 'powerline-mode-line-normal-face)
         'mouse-face 'mode-line-highlight)))

(defpowerline tl/powerline-position
  (concat
   (if (and column-number-mode line-number-mode)
       (propertize
        (format " %%l/%d %%c/%d "
                (count-lines (point-min) (point-max))
                (- (line-end-position) (line-beginning-position)))
        'local-map mode-line-column-line-number-mode-map
        'mouse-face 'mode-line-highlight
        'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")
     (if line-number-mode
         (propertize
          " L%l "
          'local-map mode-line-column-line-number-mode-map
          'mouse-face 'mode-line-highlight
          'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu")
       (if column-number-mode
           (propertize
            " C%c "
            'local-map mode-line-column-line-number-mode-map
            'mouse-face 'mode-line-highlight
            'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")
         "")))))

(defpowerline tl/powerline-file-size
  (concat
   (propertize
    " %p"
    'local-map mode-line-column-line-number-mode-map
    'mouse-face 'mode-line-highlight
    'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
   (if size-indication-mode
       (propertize
        " of %I"
        'local-map mode-line-column-line-number-mode-map
        'mouse-face 'mode-line-highlight
        'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
     "")))

(defpowerline powerline-workgroup
  (let ((str (if (fboundp 'wg-mode-line-string)
                 (wg-mode-line-string))))
    (if (> (length str) 0)
        str
      " ### ")))

(defpowerline powerline-anzu
  (if (fboundp 'anzu--update-mode-line)
      (anzu--update-mode-line)
    nil))



(defpowerline powerline-ace-window-path
  (window-parameter (selected-window) 'ace-window-path))

(defun tl--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ;; "☰" 0
   ((string= "1" str) "☱")
   ((string= "2" str) "☲")
   ((string= "3" str) "☳")
   ((string= "4" str) "☴")
   ((string= "5" str) "☵")
   ((string= "6" str) "☶")
   ((string= "7" str) "☷")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(defpowerline powerline-winum-number
  (if (and (fboundp 'winum-mode) winum-mode)
      (let* ((n (winum-get-number))
             (s (if (numberp n) (int-to-string n) "")))
        (propertize (concat " " (tl--unicode-number s))
                    'face 'winum-face))
    "#"))


;; (defpowerline powerline-winum-number
;;   (if (and (fboundp 'winum-mode) winum-mode)
;;       (let* ((n (winum-get-number))
;;              (s (if (numberp n) (int-to-string n) "")))
;;         (propertize (concat " #" s) 'face 'winum-face))
;;     "##"))


(defpowerline tl/powerline-vc
  (let ((vc-mark (char-to-string #xe0a0)))
    (if (and buffer-file-name vc-mode)
        (if (and window-system (not powerline-gui-use-vcs-glyph))
            (let ((backend (vc-backend buffer-file-name))
                  (rev (vc-working-revision buffer-file-name))
                  (disp-rev "")
                  (state (pcase (vc-state (buffer-file-name))
                           (`up-to-date "○")
                           (`edited "±")
                           (`added "+")
                           (`unregistered "?")
                           (`removed "D")
                           (`needs-merge "M")
                           (`needs-update "U")
                           (`ignored "I")
                           (_ "Unk"))))
              (when (string= "Git" backend)
                (setq backend (char-to-string #xf1d2))
                (setq disp-rev (or (vc-git--symbolic-ref buffer-file-name)
                                   (and rev (substring rev 0 7)))))
              (format " %s %s %s: %s" vc-mark backend disp-rev state))
          (format-mode-line '(vc-mode vc-mode))  )
      (format " %s untracked " vc-mark))))

(defface powerline-evil-insert-face
  '((((class color))
     :foreground "white" :background "green" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil insert state"
  :group 'powerline)

(defface powerline-evil-normal-face
  '((((class color))
     :foreground "white" :background "red" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil normal state"
  :group 'powerline)

(defface powerline-evil-visual-face
  '((((class color))
     :foreground "white" :background "orange" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil visual state"
  :group 'powerline)

(defface powerline-evil-motion-face
  '((((class color))
     :foreground "white" :background "blue" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil motion state"
  :group 'powerline)

(defface powerline-evil-emacs-face
  '((((class color))
     :foreground "white" :background "blue violet" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil emacs state"
  :group 'powerline)

(defface powerline-evil-replace-face
  '((((class color))
     :foreground "white" :background "black" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil replace state"
  :group 'powerline)

(defface powerline-evil-operator-face
  '((((class color))
     :foreground "white" :background "sky blue" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil operator state"
  :group 'powerline)


(defface powerline-evil-lispy-face
  '((((class color))
     :foreground "white" :background "orange" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil lispy state"
  :group 'powerline)

(defface powerline-evil-lisp-face
  '((((class color))
     :foreground "white" :background "orange" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil lispy state"
  :group 'powerline)


(defun powerline-evil-face (active)
  (let* ((state (if (boundp 'evil-state)
                    (symbol-name evil-state)
                  "normal"))
         (face (intern (concat "powerline-evil-" state "-face"))))
    (cond ((and active (facep face))
           face)
          (active 'powerline-active1)
          (t 'powerline-inactive1))))


(defpowerline powerline-evil-tag
  (if (bound-and-true-p evil-mode-line-tag)
      (let ((raw-tag (replace-regexp-in-string
                      "[<> «»]"
                      ""
                      (strip-text-properties evil-mode-line-tag))))
        (cond
         ((and (evil-visual-state-p) (eq evil-visual-selection 'block))
          (concat "+" raw-tag "+"))
         ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
          (concat "-" raw-tag "-"))
         (t
          (concat " " raw-tag " "))))
    " Unk "))


(defpowerline tl/powerline-client
  (if (frame-parameter nil 'client)
      "@"
    ""))

(defpowerline tl/powerline-remote
  (propertize
   (if (file-remote-p default-directory)
       "@"
     "")
   'mouse-face 'mode-line-highlight
   'help-echo (purecopy (lambda (window _object _point)
                          (format "%s"
                                  (with-selected-window window
                                    (concat
                                     (if (file-remote-p default-directory)
                                         "Current directory is remote: "
                                       "Current directory is local: ")
                                     default-directory)))))))


(defpowerline tl/powerline-frame-id
  (if (or (null window-system)
          (eq window-system 'pc))
      "-%F "
    ""))


(defvar tl/powerline-max-which-fun-length 32)
(defpowerline powerline-which-func
  (let ((s (replace-regexp-in-string
            "%" "%%"
            (or
             (gethash
              (selected-window)
              which-func-table)
             which-func-unknown))))
    (propertize
     (truncate-string-to-width s tl/powerline-max-which-fun-length nil nil "…")
     'mouse-face 'mode-line-highlight
     'help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\nmouse-3: go to end"
     'local-map which-func-keymap)))

(defpowerline powerline-recursive-left
  #("%[" 0 2
    (help-echo "Recursive edit, type C-M-c to get out")))

(defpowerline powerline-recursive-right
  #("%]" 0 2
    (help-echo "Recursive edit, type C-M-c to get out")))

(defun tl/beautify-major-mode-name (name)
  (if (stringp name)
      (cond ((string= name "Emacs-Lisp") "EL")
            ((string= name "Lisp Interaction") "IEL")
            ((string= name "IELM") "EL>")
            (t name))
    name))

(defpowerline tl/powerline-major-mode
  (propertize (format-mode-line (tl/beautify-major-mode-name mode-name))
              'mouse-face 'mode-line-highlight
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))



(defvar powerline-git-state-mark "" "git state mode line mark.")
(defpowerline powerline-git-state-mark
  powerline-git-state-mark)

(defun tl/powerline-evil-theme ()
  "Setup the default mode-line."
  (interactive)
  (let* ((active (powerline-selected-window-active))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (face3 (if active 'powerline-active3 'powerline-inactive2))
         (vc-face (if active 'powerline-vc-face 'powerline-inactive2))
         (workgroups-face (if active 'powerline-workgroups-face 'powerline-inactive2))
         (ace-window-path-face (if active
                                   'powerline-ace-window-path-face
                                 'powerline-inactive2))
         (winum-number-face (if active
                                'powerline-winum-number-face
                              'powerline-inactive2))
         (anzu-face (if active 'powerline-anzu-face 'powerline-inactive2))
         (buffer-id-face (if active 'powerline-buffer-id-face 'powerline-inactive1))
         (which-func-face (if active 'which-func 'powerline-inactive1))
         (file-base-info-face (if active
                                  'powerline-file-base-info-face
                                'powerline-inactive2))
         (evil-face (powerline-evil-face active))
         (zigzag-right (intern (format "powerline-zigzag-%s"
                                       (cdr powerline-default-separator-dir))))
         (separator-left (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir))))
         (lhs `(,(powerline-ace-window-path winum-number-face 'r)
                ,(funcall zigzag-right ace-window-path-face evil-face)
                ,(powerline-evil-tag evil-face)
                ,@(let ((anzu-info (powerline-anzu anzu-face 'l))
                        (vc-info (tl/powerline-vc vc-face 'r)))

                    (cond ((and anzu-info vc-info)
                           (list (funcall separator-left evil-face anzu-face)
                                 anzu-info
                                 (powerline-raw " " anzu-face)
                                 (funcall separator-left anzu-face vc-face)
                                 vc-info
                                 (powerline-git-state-mark vc-face)
                                 (funcall separator-left vc-face file-base-info-face)))
                          ((and anzu-info (not vc-info))
                           (list (funcall separator-left evil-face anzu-face)
                                 anzu-info
                                 (powerline-raw " " anzu-face)
                                 (funcall separator-left anzu-face file-base-info-face)))
                          ((and (not anzu-info) vc-info)
                           (list (funcall separator-left evil-face vc-face)
                                 vc-info
                                 (powerline-git-state-mark vc-face)
                                 (funcall separator-left vc-face file-base-info-face)))
                          ((and (not anzu-info) (not vc-info))
                           (list (funcall separator-left
                                          evil-face
                                          file-base-info-face)))))
                ,(powerline-raw mode-line-front-space file-base-info-face)
                ,(tl/powerline-client file-base-info-face)
                ,(tl/powerline-remote file-base-info-face)
                ,(tl/powerline-frame-id file-base-info-face)
                ,(powerline-raw mode-line-mule-info file-base-info-face)
                ,(tl/mode-line-modified file-base-info-face)
                ,(tl/powerline-position file-base-info-face)
                ,(funcall separator-left file-base-info-face buffer-id-face)
                ,(powerline-buffer-id buffer-id-face 'l)

                ,(powerline-raw ":" buffer-id-face)
                ,@(when (and (boundp 'which-function-mode) which-function-mode)
                    (list (powerline-which-func which-func-face 'l)
                          (powerline-raw " " which-func-face)))

                ,(funcall separator-left buffer-id-face face2)

                ,(when (bound-and-true-p nyan-mode)
                   (powerline-raw (list (nyan-create) face2 'l)))

                ,(powerline-raw " " face2)
                ,(powerline-raw " " face2)
                ,(powerline-recursive-left face2)
                ,(tl/powerline-major-mode face2)
                ,(powerline-process face2)
                ,(powerline-minor-modes face2 'l)
                ,(powerline-narrow face2 'l)
                ,(powerline-recursive-right face2)
                ,(powerline-raw "  " face2)
                ,(funcall separator-left face2 face2)))
         (rhs (list
               (funcall separator-right face2 workgroups-face)
               (powerline-workgroup workgroups-face)
               (powerline-raw " "  workgroups-face)

               (funcall separator-right workgroups-face face1)
               (powerline-raw "  " face1)
               (powerline-raw global-mode-string face1 'r)
               (powerline-raw " " face1)
               (funcall separator-right face1 face2)
               (tl/powerline-file-size face2 'r)
               (when powerline-display-hud (powerline-hud face2 face1)))))
    (concat (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))

;; `mode-line-format' is a a buffer-local variable. That is too say, after set
;; it with `set-default', only new created buffer will get the new value. Some
;; package such as evil or workgroups2 will update `mode-line-format' like this:
;; `(set-default 'mode-line-format (-insert-at (1+ pos) format mode-line-format))'.
;; The buffer local variable `mode-line-format' which have not update to newest
;; will issue bug.
;; So, set it as early as possible
(defun tl/setup-powerline-evil-theme ()
  "Setup the powerline evil mode-line."
  (interactive)
  (setq-default mode-line-format '("%e" (:eval (tl/powerline-evil-theme)))))

(use-package powerline-themes
  :config
  (tl/setup-powerline-evil-theme))

(defun tl/force-update-mode-line  ()
  (interactive)
  (tl/setup-powerline-evil-theme)
  (powerline-reset)
  (force-mode-line-update t)
  (let* ((buf (current-buffer))
         (fn (buffer-file-name)))
    (save-buffer buf)
    (kill-buffer buf)
    (find-file fn))
  (message "updated mode-line"))

(global-set-key (kbd "<f5>") 'tl/force-update-mode-line)


(provide '07powerline)
;;; 07powerline.el ends here
