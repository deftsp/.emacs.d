;;; 50auto-fill.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;;; AUTO FILL
;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;;(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
;; 解决段首空格缩进的问题
;; (setq adaptive-fill-mode nil)
;; put two spaces after a colon when filling.
(setq colon-double-space t)

;;; longline mode
;; (longlines-mode t) ;; soft word wrap like the rest of the world
;; (setq longlines-auto-wrap t)
;; (setq longlines-show-hard-newlines nil)

;; Visual-line-mode is a new addition that is set on by default. It is something of a replacement
;; for longlines-mode. It doesn't insert soft line breaks, so it works better on larger files, and
;; it wraps to whatever your screen is, so if you are using longlines-mode you should switch to
;; using visual-line-mode. C-n and C-p will move by visual lines, which may screw up some macros,
;; but mostly it seems to work quite well.

(setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
;; (global-visual-line-mode 1)


;;don't use auto-fill inside minibuffer
;; (add-hook 'minibuffer-setup-hook
;;           (lambda () (interactive) (auto-fill-mode -1)))

;; temp buffers resize to their text size
(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

;;; auto fill mode
(setq auto-fill-column 1
      default-fill-column 100)

(defun tsp-unfill-buffer ()
  "Undo filling for all paragraphs."
  (interactive)
  (goto-char (point-min))
  (let ((fill-column 99999))
    (fill-paragraph nil)
    (while (< (point) (point-max))
      (forward-paragraph)
      (fill-paragraph nil))))


;;------------------------------------------------------------------------------------------------------------------
;; UnfillParagraph
;;------------------------------------------------------------------------------------------------------------------
;; It works where a line ends with a newline character ("\n") and paragraphs are separated by blank lines. To make a
;; paragraph end in a single newline then use the function below:

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph Takes a multi-line paragraph and makes it into
;; a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; You can convert an entire buffer from paragraphs to lines by recording a macro that calls 'unfill-paragraph' and
;; moves past the blank-line to the next unfilled paragraph and then executing that macro on the whole buffer, 'C-u 0
;; C-x e'



;; delete the hard-wrapped line endings in a paragraph
(defun remove-hard-wrap-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(global-set-key (kbd "M-Q") 'remove-hard-wrap-paragraph)


;; 解决中英文混排不能正确fill的问题
(put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-7 'nospace-between-words t)

;;; 中英文之间自动加空格
(defun add-blank-in-chinese-and-english (&optional start end)
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

;; Function called (if non-nil) to perform auto-fill. It is called after self-inserting any character specified in the
;; `auto-fill-chars' table. NOTE: This variable is not a hook; its value may not be a list of functions.
(setq-default auto-fill-function (lambda ()
                                   (add-blank-in-chinese-and-english (point-at-bol)
                                                                     (point-at-eol))
                                   (do-auto-fill)))
