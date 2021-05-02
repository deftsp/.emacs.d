;;; 50slime-js.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: abbrev, abbrev

;;; install
;; npm install swank-js
;; M-x el-get-install swank-js

;; (require 'slime)
;; (require 'slime-js)
;; (require 'js2-refactor)

;; (set-default 'slime-js-connect-url "http://localhost:8009")
;; (set-default 'slime-js-starting-url "/")
(set-default 'slime-js-swank-command "swank-js")
(set-default 'slime-js-swank-args '())
;; (set-default 'slime-js-browser-command "open -a \"Google Chrome\"")
;; (set-default 'slime-js-browser-jacked-in-p nil)

(eval-after-load "slime-js"
  '(progn
     (define-key slime-js-minor-mode-map (kbd "C-x C-e") 'slime-js-eval-current)
     (define-key slime-js-minor-mode-map (kbd "C-c C-e") 'slime-js-eval-and-replace-current)
     (define-key slime-js-minor-mode-map [f5] 'slime-js-reload)))

(defun tl/slime-js-run-swank ()
  "Runs the swank side of the equation."
    (interactive)
    (let ((slime-js-swank-command "swank-js")
          (slime-js-swank-args '()))
      (setq slime-js-swank-buffer (apply #'make-comint
                                         "swank-js"
                                         slime-js-swank-command
                                         nil
                                         slime-js-swank-args))))

(defun tl/slime-js-jack-in-node ()
  "Start a swank-js server and connect to it, opening a repl."
  (interactive)
  (let ((slime-protocol-version 'ignore))
    (tl/slime-js-run-swank)
    (sleep-for 2)
    (slime-connect "localhost" 4005)))

(defun tl/slime-js-jack-in-browser ()
  "Start a swank-js server, connect to it, open a repl, open a browser, connect to that."
  (interactive)
  (let ((slime-js-target-url "http://localhost:3000")
        (slime-js-browser-command "open -a \"Google Chrome\"")
        (slime-js-starting-url "/swank-js/test.html")
        (slime-js-connect-url "http://localhost:8009"))
   (tl/slime-js-jack-in-node)
   (sleep-for 2)
   (slime-js-set-target-url slime-js-target-url)
   (shell-command (concat slime-js-browser-command " " slime-js-connect-url slime-js-starting-url))
   (sleep-for 3)
   (setq slime-remote-history nil)
   (slime-js-sticky-select-remote (caadr (slime-eval '(js:list-remotes))))
   (setq slime-js-browser-jacked-in-p t)))

;; (defadvice save-buffer (after save-css-buffer activate)
;;   (when (and slime-js-browser-jacked-in-p (eq major-mode 'css-mode))
;;     (slime-js-refresh-css)))

(defun js2-eval-friendly-node-p (n)
  (or (and (js2-stmt-node-p n) (not (js2-block-node-p n)))
      (and (js2-function-node-p n) (js2-function-node-name n))))

(defun slime-js--echo-result (result &rest _)
  (message result))

(defun slime-js--replace-with-result (replacement beg end)
  (save-excursion
    (goto-char beg)
    (delete-char (- end beg))
    (insert replacement)))

(defun slime-js-eval-region (beg end &optional func)
  (lexical-let ((func (or func 'slime-js--echo-result))
                (beg beg)
                (end end))
    (slime-flash-region beg end)
    (slime-js-eval
     (buffer-substring-no-properties beg end)
     #'(lambda (s) (funcall func (cadr s) beg end)))))

(defun slime-js-eval-statement (&optional func)
  (let ((node (js2r--closest 'js2-eval-friendly-node-p)))
    (slime-js-eval-region (js2-node-abs-pos node)
                          (js2-node-abs-end node)
                          func)))

(defun slime-js-eval-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark))
      (slime-js-eval-statement)))

(defun slime-js-eval-and-replace-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark) 'slime-js--replace-with-result)
      (slime-js-eval-statement 'slime-js--replace-with-result)))


;;; work with js2-mode
(defun tl/enable-slime-js-minor-mode ()
  (when (fboundp 'slime-js-minor-mode)
    (slime-js-minor-mode +1)))

(eval-after-load "js2-mode"
  '(progn
    (add-hook 'js2-mode-hook 'tl/enable-slime-js-minor-mode)))

;;; work with css mode
(defun tl/slime-js-css-setup ()
  (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
  (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css))

(eval-after-load "css-mode"
  '(progn
    (add-hook 'css-mode-hook 'tl/slime-js-css-setup)))

;; Remove slime-minor-mode from mode line if diminish.el is installed
(when (boundp 'diminish)
  (diminish 'slime-js-minor-mode))

;;; ac-slime
;; https://github.com/purcell/ac-slime
(eval-after-load "slime"
'(when (fboundp 'set-up-slime-ac)
   (add-hook 'slime-mode-hook 'set-up-slime-ac)
   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))

(provide '50slime-js)
;;; 50slime-js.el ends here
