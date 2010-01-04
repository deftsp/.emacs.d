;;; 00util.el --- ome useful, standalone utilities

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

(defmacro aif (&rest forms)
  "Create an anonymous interactive function.
    Mainly for use when binding a key to a non-interactive function."
  `(lambda () (interactive) ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-soft  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

;;;;;;;;;;;;

(defun mt-to-omr (start end)
  "Change muse tag to org mark rule"
  (interactive "r")
  (save-excursion
    (format-replace-strings '(("<example>" . "#+BEGIN_EXAMPLE")
                              ("</example>" . " #+END_EXAMPLE ")
                              ("<code>" . " #+BEGIN_EXAMPLE ")
                              ("</code>" . " #+END_EXAMPLE ")
                              ("<src lang=\"shell-script\">" . "#+BEGIN_SRC shell-script")
                              ("<src lang=\"emacs-lisp\"" . "#+BEGIN_SRC emacs-lisp")
                              ("<src lang=\"conf\"" . "#+BEGIN_SRC conf")
                              ("<src lang=\"lisp\"" . "#+BEGIN_SRC lisp")
                              ("<src lang=\"c\"" . "#+BEGIN_SRC c")
                              ("<src lang=\"c++\"" . "#+BEGIN_SRC c++")
                              ("</src>" . "#+END_SRC"))
                            nil
                            start
                            end)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_EXAMPLE.*$/#+BEGIN_EXAMPLE/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+END_EXAMPLE.*$/#+END_EXAMPLE/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_SRC\ .*shell.*$/#+BEGIN_SRC shell-script/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_SRC\ .*emacs.*$/#+BEGIN_SRC emacs-lisp/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_SRC\ .*lisp.*$/#+BEGIN_SRC lisp/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_SRC\ .*c$/#+BEGIN_SRC c/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+BEGIN_SRC\ .*conf.*$/#+BEGIN_SRC conf/g'" nil t)
    (shell-command-on-region start end "sed 's/^.*#\+END_SRC.*$/#+END_SRC/g'" nil t)))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full-screen mode
;; based on http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;; toggle full screen with F11; require 'wmctrl'
;; http://stevenpoole.net/blog/goodbye-cruel-word/

;; (when (executable-find "wmctrl") ; apt-get install wmctrl
;;   (defun full-screen-toggle ()
;;     (interactive)
;;     (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
;;   (global-set-key (kbd "<f11>")  'full-screen-toggle))

;; (global-set-key [f11]
;;       (lambda ()
;;         (interactive)
;;         (x-send-client-message
;;          nil 0 nil "_NET_WM_STATE" 32
;;          '(2 "_NET_WM_STATE_FULLSCREEN" 0))))

;; maximal
;; (defun my-maximized-horz ()
;;   (interactive)
;;   (x-send-client-message
;;    nil 0 nil "_NET_WM_STATE" 32
;;    '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
;; (defun my-maximized-vert ()
;;   (interactive)
;;   (x-send-client-message
;;    nil 0 nil "_NET_WM_STATE" 32
;;    '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;; (defun my-maximized ()
;;   (interactive)
;;   (x-send-client-message
;;    nil 0 nil "_NET_WM_STATE" 32
;;    '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;   (x-send-client-message
;;    nil 0 nil "_NET_WM_STATE" 32
;;    '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))


(defun DE-add-line-spacing (&optional spacing start end force)
  "Add line SPACING to each newline of region START END.
If SPACING is omitted, remove line-height property of all newlines.
If region START END is not specified, use whole current buffer.
If FORCE is non-nil, overwrite any existing line-height properties."
  (interactive)
  (let ((height (progn (redisplay) (car (window-line-height)))))
    (unless (and start end)
      (setq start (point-min)
            end (point-max)))
    (save-excursion
      (goto-char start)
      (while (search-forward "\n" end t)
        (if spacing
            (when (or force
                     (null (get-text-property (1- (point)) 'line-height)))
              (replace-match (propertize "\n" 'line-height (+ height spacing))))
            (remove-text-properties (1- (point)) (point) '(line-height)))))))

;;; copy
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))


(defun copy-word (&optional arg)
  "Copy words at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
        (end (progn (forward-word arg) (point))))
    (copy-region-as-kill beg end)))

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end)))

;;; Towards smoother scrolling in Emacs
;; http://www.randomsample.de/dru5/node/25
(defun DE-visual-scroll-up (&optional arg)
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (message "End of buffer")
      (unless arg
        (setq arg 1))
      (let ((cur (point))
            pos visible)
        (setq pos
              (save-excursion
                (while (and (search-forward "\n" nil t)
                          (= (length (pos-visible-in-window-p
                                      (point) nil t)) 2)))
                (1- (point))))
        (setq visible
              (pos-visible-in-window-p pos nil t))
        ;; if point is fully visible, we can go there
        (when (and (= (length visible) 2)
                 (not (= pos cur)))
          (goto-char pos))
        ;; if point is partly visible, we only go there if we absolutely
        ;; have to (point is already at the top)
        (when (and (= pos cur)
                 (null (pos-visible-in-window-p (1- (point)))))
          (forward-line 1))
        (set-window-vscroll nil (+ (window-vscroll) arg)))))

(defun DE-visual-scroll-down (&optional arg)
  (interactive)
  (if (pos-visible-in-window-p (point-min))
      (message "Beginning of buffer")
      (unless arg
        (setq arg 1))
      (let ((cur (point))
            pos visible)
        (setq pos
              (save-excursion
                (while (and (search-backward "\n" nil t)
                          (= (length (pos-visible-in-window-p (point) nil t)) 2)))
                (+ 1 (point))))
        (setq visible
              (pos-visible-in-window-p pos nil t))
        (when (and (= (length visible) 2)
                 (not (= pos cur)))
          (goto-char pos))
        (when (and (= pos cur)
                 (null (pos-visible-in-window-p
                        (save-excursion (forward-line 1) (point)))))
          (goto-char (1- (point))))
        (when (zerop (window-vscroll))
          (message "vscroll is 0. Reverting to scroll-down.")
          (scroll-down arg))
        (set-window-vscroll nil (- (window-vscroll) arg)))))


(defun fcitx-mb-eim ()
  (interactive)
  (while (search-forward-regexp "^\\([a-z;',./]*\\)\\ " nil t)
    (let ((str (match-string 1)))
      (replace-match (concatenate 'string "(\"" str "\" "))))

  ;; (goto-char (point-min))
  ;; (while (search-forward-regexp "^\\(.*\\)\\ \\(.*\\)\\ \\(.*\\)$" nil t)
  ;;   (let ((str1 (match-string 1))
  ;;         (str2 (match-string 2))
  ;;         (str3 (match-string 3)))
  ;;     (replace-match (concatenate 'string "(\"" str1 "\" " "\"" str2 "\" " "\"" str3 "\")"))))

  (goto-char (point-min))
  (while (search-forward-regexp "\\ \\(\\cc*\\)" nil t)
    (let ((str (match-string 1)))
      (replace-match (concatenate 'string " \"" str "\""))))

  (goto-char (point-min))
  (while (search-forward-regexp "\\(.$\\)" nil t)
    (let ((str (match-string 1)))
      (replace-match (concatenate 'string str ")")))))

