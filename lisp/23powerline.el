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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/powerline"))
(require 'powerline nil t)

(defface powerline-wg-face '((t (:background "#009956" :foreground "#0a3540")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-vc-face '((t (:background "#849c10" :foreground "#0a3540")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-file-base-info-face '((t (:background "#3d89d0" :foreground "#0a3540")))
  "Powerline face 3."
  :group 'powerline)


(defun strip-text-properties(text)
  (set-text-properties 0 (length text) nil text)
  text)

(defpowerline pl/powerline-position
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

(defpowerline pl/powerline-file-size
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

(defpowerline pl/powerline-workgroup
  (if (and (boundp 'workgroups-mode)
           (symbol-value 'workgroups-mode))
      (wg-mode-line-string)
    " ### "))

(defpowerline pl/powerline-ace-window-show-key
  (when aw-mode-line-show-key
    (aw-mode-line-key-string)))

(defpowerline pl/powerline-vc
  (let ((vc-mark (char-to-string #xe0a0)))
    (if (and (buffer-file-name (current-buffer)) vc-mode)
        (if window-system
            (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
              (when backend
                (concat " " vc-mark " " (vc-working-revision (buffer-file-name (current-buffer)) backend))))
          (format-mode-line '(vc-mode vc-mode)))
      (concat " " vc-mark " untracked "))))

(defpowerline pl/powerline-evil-tag
  (let* ((raw-text (strip-text-properties evil-mode-line-tag))
         (raw-tag (replace-regexp-in-string "[<> «»]" "" raw-text)))
    (cond
     ((and (evil-visual-state-p) (eq evil-visual-selection 'block))
      (concat "+" raw-tag "+"))
     ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
      (concat "-" raw-tag "-"))
     (t
      (concat " " raw-tag " ")))))

(defun pl/powerline-evil-theme ()
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
             (wg-face (if active 'powerline-wg-face 'powerline-inactive2))
             (file-base-info-face (if active 'powerline-file-base-info-face 'powerline-inactive2))
             (evil-face (powerline-evil-face active))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (lhs `(,(pl/powerline-workgroup wg-face)
                    ,(funcall separator-left wg-face evil-face)
                    ;; ,(pl/powerline-ace-window-show-key face1 'r)
                    ;; ,(funcall separator-left face1 evil-face)
                    ;; ,(pl/powerline-evil-tag evil-face)
                    ,@(let ((vc-info (pl/powerline-vc vc-face 'r)))
                        (if vc-info
                            (list (funcall separator-left evil-face vc-face)
                                  vc-info
                                  (funcall separator-left vc-face file-base-info-face))
                          (list (funcall separator-left evil-face file-base-info-face))))

                    ,(powerline-raw mode-line-front-space file-base-info-face 'l)
                    ,(powerline-raw mode-line-mule-info file-base-info-face)
                    ,(powerline-raw mode-line-modified file-base-info-face)
                    ,(powerline-client file-base-info-face)
                    ,(powerline-remote file-base-info-face)
                    ,(powerline-frame-id file-base-info-face)
                    ,(powerline-buffer-id file-base-info-face 'l)
                    ,(funcall separator-left file-base-info-face face2)
                    ,(pl/powerline-position face2)

                    ,(powerline-raw " " file-base-info-face)
                    ,@(when (and (boundp 'which-function-mode) which-function-mode)
                        (list (funcall separator-left face2 face1)
                              (powerline-which-func face1 'l)
                              (powerline-raw " " face1)
                              ;; (funcall separator-left face1 wg-face)
                              ))
                    ;; ,(funcall separator-left wg-face face2)
                    ;; ,(when (boundp 'erc-modified-channels-object)
                    ;;    (powerline-raw erc-modified-channels-object face1 'l))
                    ,(powerline-raw " " face2)
                    ;; ,(powerline-raw
                    ;;   (if (and (boundp 'mode-line-debug-mode) mode-line-debug-mode)
                    ;;       (mode-line-debug-control)
                    ;;     " ")
                    ;;   face1)
                    ,(powerline-recursive-left face2)
                    ,(powerline-major-mode face2)
                    ,(powerline-process face2)
                    ,(powerline-minor-modes face2 'l)
                    ,(powerline-narrow face2 'l)
                    ,(powerline-recursive-right face2)
                    ,(powerline-raw "  " face2)
                    ,(funcall separator-left face2 face2)))
             (rhs (list
                   (funcall separator-right face2 face1)
                   (powerline-raw "  " face1)
                   (powerline-raw global-mode-string face1 'r)
                   (powerline-raw " " face1)
                   (funcall separator-right face1 face2)
                   (pl/powerline-file-size face2 'r)
                   (when powerline-use-hud (powerline-hud face2 face1)))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))


(with-eval-after-load 'powerline-themes
  (setq powerline-default-separator 'utf-8) ; 'arrow
  (pl/powerline-evil-theme))

(defun pl/force-update-mode-line  ()
  (interactive)
  (pl/powerline-evil-theme)
  (powerline-reset)
  (force-mode-line-update t)
  (let* ((buf (current-buffer))
         (fn (buffer-file-name)))
    (save-buffer buf)
    (kill-buffer buf)
    (find-file fn))
  (message "updated mode-line"))

(global-set-key (kbd "<f5>") 'pl/force-update-mode-line)


(provide '23powerline)
;;; 23powerline.el ends here
