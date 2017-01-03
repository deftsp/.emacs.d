;;; 23powerline.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
;; currently when ns-use-srgb-colorspace is t separator color are wrong
;; see https://github.com/milkypostman/powerline/issues/54.

;; install this fork https://github.com/unic0rn/powerline instead, and installed
;; patched fonts from https://github.com/Lokaltog/powerline-fonts .

;; vc.*face inherit the face of mode-line, it cause powerline-vc can not change
;; it's background and foreground. Defining vc-state-base-face not to inherit
;; will slove that.
(require 'powerline nil t)
(require 's)

(defvar powerline-git-state-mark-modeline t
  "When t git state mark on will work with powrline instead of in the front of
the modeline")

(defface powerline-workgroups-face
  '((t (:background "#778899" :foreground "#0a3540")))
  "Powerline workgroups face."
  :group 'powerline)

(defface powerline-vc-face
  '((t (:background "#849c10" :foreground "#0a3540")))
  "Powerline vc face."
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
(defpowerline paloryemacs/mode-line-modified
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

(defpowerline paloryemacs/powerline-position
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
          " L%l"
          'local-map mode-line-column-line-number-mode-map
          'mouse-face 'mode-line-highlight
          'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu")
       (if column-number-mode
           (propertize
            " C%c"
            'local-map mode-line-column-line-number-mode-map
            'mouse-face 'mode-line-highlight
            'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")
         "")))))

(defpowerline paloryemacs/powerline-file-size
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
  (if (and (boundp 'workgroups-mode)
           (symbol-value 'workgroups-mode))
      (wg-mode-line-string)
    " ### "))

(defpowerline powerline-ace-window-path
  (window-parameter (selected-window) 'ace-window-path))

(defpowerline paloryemacs/powerline-vc
  (let ((vc-mark (char-to-string #xe0a0)))
    (if (and buffer-file-name vc-mode)
        (if (and window-system (not powerline-gui-use-vcs-glyph))
            (format " %s %s: %s"
                    vc-mark
                    (vc-backend buffer-file-name)
                    (s-left 7 (vc-working-revision buffer-file-name)))
          (format-mode-line '(vc-mode vc-mode)))
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

(defface powerline-evil-iedit-face
  '((((class color))
     :foreground "blue" :background "yellow" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil iedit state"
  :group 'powerline)

(defface powerline-evil-evilified-face
  '((((class color))
     :foreground "#232323" :background "DarkOrchid2" :weight bold :inherit mode-line)
    (t (:weight bold)))
  "face to fontify evil evilified state"
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
  (if (and (boundp 'evil-mode) evil-mode)
      (let* ((raw-text (strip-text-properties evil-mode-line-tag))
             (raw-tag (replace-regexp-in-string "[<> «»]" "" raw-text)))
        (cond
         ((and (evil-visual-state-p) (eq evil-visual-selection 'block))
          (concat "+" raw-tag "+"))
         ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
          (concat "-" raw-tag "-"))
         (t
          (concat " " raw-tag " "))))
    " NIL "))


(defpowerline paloryemacs/powerline-client
  (if (frame-parameter nil 'client)
      "@"
    ""))

(defpowerline paloryemacs/powerline-remote
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


(defpowerline paloryemacs/powerline-frame-id
  (if (or (null window-system)
          (eq window-system 'pc))
      "-%F "
    ""))


(defpowerline powerline-which-func
  (propertize
   (replace-regexp-in-string "%" "%%"
                             (or
                              (gethash
                               (selected-window)
                               which-func-table)
                              which-func-unknown))
   'mouse-face 'mode-line-highlight
   'help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\nmouse-3: go to end"
   'local-map which-func-keymap))

(defpowerline powerline-recursive-left
  #("%[" 0 2
    (help-echo "Recursive edit, type C-M-c to get out")))

(defpowerline powerline-recursive-right
  #("%]" 0 2
    (help-echo "Recursive edit, type C-M-c to get out")))

(defvar powerline-git-state-mark "" "git state mode line mark.")
(defpowerline powerline-git-state-mark
  powerline-git-state-mark)

(defun paloryemacs/powerline-evil-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (face3 (if active 'powerline-active3 'powerline-inactive2))
             (vc-face (if active 'powerline-vc-face 'powerline-inactive2))
             (workgroups-face (if active 'powerline-workgroups-face 'powerline-inactive2))
             (buffer-id-face (if active 'powerline-buffer-id-face 'powerline-inactive1))
             (which-func-face (if active 'which-func 'powerline-inactive1))
             (file-base-info-face (if active 'powerline-file-base-info-face 'powerline-inactive2))
             (evil-face (powerline-evil-face active))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (lhs `(,(powerline-evil-tag evil-face)
                    ,@(let ((vc-info (paloryemacs/powerline-vc vc-face 'r)))
                        (if vc-info
                            (list (funcall separator-left evil-face vc-face)
                                  vc-info
                                  (powerline-git-state-mark vc-face)
                                  (funcall separator-left vc-face file-base-info-face))
                          (list (funcall separator-left
                                         evil-face
                                         file-base-info-face))))

                    ,(powerline-raw mode-line-front-space file-base-info-face)
                    ,(paloryemacs/powerline-client file-base-info-face)
                    ,(paloryemacs/powerline-remote file-base-info-face)
                    ,(paloryemacs/powerline-frame-id file-base-info-face)
                    ,(powerline-raw mode-line-mule-info file-base-info-face)
                    ,(paloryemacs/mode-line-modified file-base-info-face)
                    ,(paloryemacs/powerline-position file-base-info-face)
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
                    ,(powerline-major-mode face2)
                    ,(powerline-process face2)
                    ,(powerline-minor-modes face2 'l)
                    ,(powerline-narrow face2 'l)
                    ,(powerline-recursive-right face2)
                    ,(powerline-raw "  " face2)
                    ,(funcall separator-left face2 face2)))
             (rhs (list
                   (funcall separator-right face2 workgroups-face)
                   (powerline-workgroup workgroups-face)
                   (powerline-raw ": "  workgroups-face)
                   (powerline-ace-window-path workgroups-face 'r)

                   (funcall separator-right workgroups-face face1)
                   (powerline-raw "  " face1)
                   (powerline-raw global-mode-string face1 'r)
                   (powerline-raw " " face1)
                   (funcall separator-right face1 face2)
                   (paloryemacs/powerline-file-size face2 'r)
                   (when powerline-display-hud (powerline-hud face2 face1)))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))


(with-eval-after-load 'powerline-themes
  ;; (setq powerline-default-separator 'utf-8) ; 'arrow
  (paloryemacs/powerline-evil-theme))

(defun paloryemacs/force-update-mode-line  ()
  (interactive)
  (paloryemacs/powerline-evil-theme)
  (powerline-reset)
  (force-mode-line-update t)
  (let* ((buf (current-buffer))
         (fn (buffer-file-name)))
    (save-buffer buf)
    (kill-buffer buf)
    (find-file fn))
  (message "updated mode-line"))

(global-set-key (kbd "<f5>") 'paloryemacs/force-update-mode-line)


(provide '23powerline)
;;; 23powerline.el ends here
