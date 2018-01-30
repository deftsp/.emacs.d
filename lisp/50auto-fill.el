;;; 50auto-fill.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;; (setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
;; (setq adaptive-fill-mode nil) ; 解决段首空格缩进的问题
(setq colon-double-space t) ; put two spaces after a colon when filling.

;; temp buffers resize to their text size
(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

;;; auto fill mode
(setq-default fill-column 80)

(defun paloryemacs/unfill-buffer ()
  "Undo filling for all paragraphs."
  (interactive)
  (goto-char (point-min))
  (let ((fill-column 99999))
    (fill-paragraph nil)
    (while (< (point) (point-max))
      (forward-paragraph)
      (fill-paragraph nil))))

(use-package fill-column-indicator
  :init
  (progn
    (setq fci-rule-width 2
          fci-dash-pattern 0.75
          ;; fci-rule-color "#586e75" ; set by theme
          fci-rule-use-dashes t)))

;; unfill paragraph
;; It works where a line ends with a newline character ("\n") and paragraphs are separated by blank lines. To make a
;; paragraph end in a single newline then use the function below:

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph Takes a multi-line paragraph and makes it into
;; a single line of text.
;; http://pages.sachachua.com/.emacs.d/Sacha.html
(defun paloryemacs/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-q") 'paloryemacs/fill-or-unfill-paragraph) ;; `M-q' default to `fill-paragraph'

;; You can convert an entire buffer from paragraphs to lines by recording a macro that calls 'paloryemacs/unfill-paragraph' and
;; moves past the blank-line to the next unfilled paragraph and then executing that macro on the whole buffer, 'C-u 0
;; C-x e'

;; delete the hard-wrapped line endings in a paragraph
(defun paloryemacs/remove-hard-wrap-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun paloryemacs/remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(global-set-key (kbd "M-Q") 'paloryemacs/remove-hard-wrap-paragraph)


;; 解决中英文混排不能正确 fill 的问题
(put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-7 'nospace-between-words t)

(defun paloryemacs/add-blank-between-chinese-and-english (&optional start end)
  "automaticall add a blank between English and Chinese words."
  (interactive)
  (save-excursion
    (progn
      (if (not start)
          (setq start (point-min)))
      (if (not end)
          (setq end (point-max)))
      (goto-char start)
      (while (and (re-search-forward "\\(\\cc\\)\\([0-9-]*[a-z]\\)"  nil t)
                  (<= (match-end 0) end ))
        (replace-match "\\1 \\2" nil nil))
      (goto-char start)
      (while (and (re-search-forward "\\([a-z][0-9-]*\\)\\(\\cc\\)"  nil t)
                  (<= (match-end 0) end ))
        (replace-match "\\1 \\2" nil nil)))))

;; Function called (if non-nil) to perform auto-fill. It is called after
;; self-inserting any character specified in the `auto-fill-chars' table. NOTE:
;; This variable is not a hook; its value may not be a list of functions.
(defun paloryemacs/add-blank-between-chinese-and-english-whole-buffer ()
  (interactive)
  (paloryemacs/add-blank-between-chinese-and-english (point-at-bol) (point-at-eol))
  (do-auto-fill))

(setq-default auto-fill-function
              #'paloryemacs/add-blank-between-chinese-and-english-whole-buffer)


(use-package prog-fill
  :commands (prog-fill)
  :defer t
  :init
  (progn
    ;; (add-hook 'prog-mode-hook
    ;;           (lambda () (local-set-key (kbd "M-q") #'prog-fill)))
    ))

(provide '50auto-fill)
