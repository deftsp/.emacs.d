;;; 50window.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(setq switch-to-buffer-preserve-window-point t)

;;; key binding
(global-set-key (kbd"C-x x") 'delete-window)
(when window-system
  (global-set-key (kbd "M-`") 'other-window))


;;; adjust window size
;; enlarge-window                       Ctrl+x ^     increase height
;; shrink-window                                     decrease height
;; enlarge-window-horizontally          Ctrl+x }     increase width
;; shrink-window-horizontally           Ctrl+x {     decrease width
;; shrink-window-if-larger-than-buffer  Ctrl+x -	 shrink a window to fit its content.
;; balance-windows                      Ctrl+x +	 make all windows same width/height


(global-set-key (kbd "H-[") 'shrink-window)
(global-set-key (kbd "H-]") 'enlarge-window)
(global-set-key (kbd "H-M-[") 'shrink-window-horizontally)
(global-set-key (kbd "H-M-]") 'enlarge-window-horizontally)

;;; scroll other window
;; C-M-v   scroll-other-window
;; C-M-S-v scroll-other-window-down  ; use 'C-h b' to found it
(global-set-key (kbd "H-M-,") '(lambda() (interactive) (scroll-other-window -1)))
(global-set-key (kbd "H-M-.") '(lambda() (interactive) (scroll-other-window 1)))

;;; toggle window dedicaton
;; set-window-dedicated-p
;; When a window is dedicated to its buffer, `display-buffer' will refrain
;; from displaying another buffer in it.
(defun pl/toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(define-key ctl-x-4-map (kbd "C-d") 'pl/toggle-current-window-dedication)

;;; window extension
;; http://www.emacswiki.org/emacs/window-extension.el
;; sticky-window-delete-other-windows

;;; winner mode
(winner-mode t)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)
(global-set-key (kbd "H-u") 'winner-undo)
(global-set-key (kbd "H-r") 'winner-redo)

;;; windmove
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-B") 'windmove-left)
(global-set-key (kbd "M-F") 'windmove-right)


;;; switch between horizontal and vertical layout of two windows
;; base on http://whattheemacsd.com/buffer-defuns.el-03.html
(global-set-key (kbd "C-x !") 'pl/toggle-window-split)
(defun pl/toggle-window-split ()
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((this-win-buffer (window-buffer))
                (next-win-buffer (window-buffer (next-window)))
                (this-win-edges (window-edges (selected-window)))
                (next-win-edges (window-edges (next-window)))
                (this-win-2nd (not (and (<= (car this-win-edges)
                                            (car next-win-edges))
                                        (<= (cadr this-win-edges)
                                            (cadr next-win-edges)))))
                (splitter
                 (if (= (car this-win-edges)
                        (car (window-edges (next-window))))
                     'split-window-horizontally
                   'split-window-vertically)))
           (delete-other-windows)
           (let ((first-win (selected-window)))
             (funcall splitter)
             (if this-win-2nd (other-window 1))
             (set-window-buffer (selected-window) this-win-buffer)
             (set-window-buffer (next-window) next-win-buffer)
             (select-window first-win)
             (if this-win-2nd (other-window 1)))))))

(defun pl/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(defun pl/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(global-set-key (kbd "C-x 2") 'pl/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'pl/hsplit-last-buffer)


;;; for wide-screen display
;; http://emacswiki.org/emacs/display-buffer-for-wide-screen.el
;; minimum width of window to be split horizontally for `display-buffer'
(setq split-width-threshold 200
      split-height-threshold 20)

;; (defvaralias 'split-window-horizontally-threshold-width 'split-width-threshold)

;;; This function is originally written by Tassilo Horn.
;;; Rubikitch modified slightly.
;;; http://www.mail-archive.com/emacs-pretest-bug@gnu.org/msg11469.html
;; (defun display-buffer-function-according-to-window-width (buffer force-other-window &rest ignored)
;;   "If BUFFER is visible, select it.

;; If it's not visible and there's only one window, split the
;; current window and select BUFFER in the new window. If the
;; current window (before the split) is more than
;; `split-window-horizontally-threshold-width' columns wide,
;; split horizontally, else split vertically.

;; If the current buffer contains more than one window, select
;; BUFFER in the least recently used window.

;; This function returns the window which holds BUFFER.

;; FORCE-OTHER-WINDOW is ignored."
;;   (or (get-buffer-window buffer)
;;       (and special-display-function
;;            (or (member (buffer-name buffer) special-display-buffer-names)
;;                (some (lambda (re) (string-match re (buffer-name buffer))) special-display-regexps))
;;            (funcall special-display-function buffer))
;;       (if (one-window-p)
;;           (let ((new-win (if (> (window-width) (or split-window-horizontally-threshold-width 160)) ;originally 165
;;                              (split-window-horizontally)
;;                            (split-window-vertically))))
;;             (set-window-buffer new-win buffer)
;;             new-win)
;;         (let ((new-win (get-lru-window)))
;;           (set-window-buffer new-win buffer)
;;           new-win))))

;; (setq display-buffer-function 'display-buffer-function-according-to-window-width)

;;; Ctrl-x +:  balance-windows -- makes all visible windows approximately equal height. This is useful if you've just
;; done Ctrl-x 2 twice in a row, because you'll have two 1/4-height windows and one 1/2-height window. Ctrl-x + makes
;; them all the same height.

;;;;;;;;;;;;;;;;;

;;; special window
;; thanks to http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; (mapcar #'(lambda (buffer-name)
;;             (add-to-list 'special-display-buffer-names buffer-name))
;;         (list "*Ido Completions*" "*Completions*"))

;; (setq special-display-function 'pl/display-special-buffer-popup)
;; (add-to-list 'special-display-regexps ".*")  ; match any window

;; (defun pl/display-special-buffer-popup (buffer &optional args)
;;   "put the special buffers in the bottom right"
;;   ;; The top left corner of the frame is considered to be row 0,
;;   ;; column 0.
;;   (let* ((target-window (window-at 6 (- (frame-height) 6)))
;;          (pop-up-windows t)
;;          (window (window--try-to-split-window target-window)))
;;     (set-window-buffer window buffer)))

;;; recursive-edit
;; Hitting key chord ",r" will put you in a "recursive editing mode", that is simply an embedded call to the editing loop. The
;; point here is that you can exit this inner loop, which means that you return from the recursive-edit function. This
;; way, the recursive editing can be guarded by some context-saving macros : here save-window-excursion and
;; save-excursion. Once the user quits the recursive edit, the context is restored, which means here that the windows
;; state, current buffer and position are restored : you're back in the state where your brain was preempted without
;; even needing to remember it.

;; Enter a recursive edit. C-M-c will bring back exactly there
;; `C-M-c' default binding to exit-recursive-edit, it means returning to the
;; unfinished command, which continues execution
;; `C-]'   default binding to abort-recursive-edit, This is like exiting, but
;; also quits the unfinished command immediately.

;;; ",r" key-chord
(defun pl/recursive-edit-save-window-config ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (recursive-edit))))

;; RecursiveEditPreservingWindowConfig
;; One can change the window configuration temporarily using RecursiveEdit?.
;; Inspired by a command posted by ErikNaggum in an Emacs Newsgroup, EmilioLopes
;; wrote this macro:

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro pl/recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with \\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `pl/recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; Use it like this:

(global-set-key (kbd "C-c 0") (pl/recursive-edit-preserving-window-config (delete-window)))
(global-set-key (kbd "C-c 2") (pl/recursive-edit-preserving-window-config
                               (split-window-vertically 20)))
(global-set-key (kbd "C-c 3") (pl/recursive-edit-preserving-window-config
                               (split-window-horizontally -52)))
(global-set-key (kbd "C-c 1") (pl/recursive-edit-preserving-window-config
                               (if (one-window-p 'ignore-minibuffer)
                                   (message "Current window is the only window in its frame")
                                 (delete-other-windows))))

;; Now pressing "C-c 1" will delete all other windows in the current frame and put
;; you into "recursive editing". You know you are in a recursive edit by noting the
;; square brackets around the parentheses that always surround the major and minor
;; mode names. After exiting recursive edit, e.g. by using "C-M-c"
;; ('exit-recursive-edit'), the original window configuration is restored.

;; recursive-edit end there ---------------------------------------------------------------

;;; ace-window
;; swap windows by calling ace-window with a prefix argument C-u.
;; delete the selected window by calling ace-window with a double prefix argument, i.e. C-u C-u.
;; not use switch-window anymore, ace-window is faster
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; TODO: Fix it
;; (require 'ace-window-mode-line nil t)
;; (setq ace-window-mode-line-format "⌗%c"
;;       ace-window-mode-line-position 1)

;; (defun pl/turn-on-ace-window-show-key ()
;;   (when (fboundp 'ace-window-mode-line-show-key)
;;     (ace-window-mode-line-show-key +1)))
;; make sure ace window key to the right position
;; (add-to-list 'after-init-hook 'pl/turn-on-ace-window-show-key)

;;; golden-ratio.el
;; (require 'golden-ratio nil t)
;; (eval-after-load "golden-ratio"
;;   '(progn
;;      (golden-ratio-mode -1)
;;      (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-select-buffer-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "org-mode")
;;      (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
;;      (require 'ediff nil t)
;;      (if (boundp 'ediff-this-buffer-ediff-sessions)
;;          (add-to-list 'golden-ratio-inhibit-functions 'pl/ediff-comparison-buffer-p))))

(defun pl/ediff-comparison-buffer-p ()
  ediff-this-buffer-ediff-sessions)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;;; buffer-move
;; swap buffers without typing C-x b on each window
(eval-after-load "buffer-move"
  '(progn
     (global-set-key (kbd "M-[ p") 'buf-move-up)
     (global-set-key (kbd "M-[ n") 'buf-move-down)
     (global-set-key (kbd "M-[ b") 'buf-move-left)
     (global-set-key (kbd "M-[ f") 'buf-move-right)))

;;; popwin
;; https://github.com/m2ym/popwin-el
;; (require 'popwin nil t)
;; (eval-after-load "popwin"
;;   ;; (global-set-key (kbd "C-z") popwin:keymap)
;;   ;; (add-to-list 'popwin:special-display-config)
;;   (popwin-mode 1))

;;; win-switch
(require 'win-switch nil t)

(global-set-key "\M-o" 'win-switch-dispatch)

(defface pl/win-switch-face-background
  '((t (:foreground "#586e75")))
  "Face for background of win-switch")

(defvar pl/win-switch-face-overlay nil)

(defun pl/win-switch-on-feedback-func ()
  (let ((ol (make-overlay (point-min) (point-max)
                          (current-buffer) nil t)))
    (overlay-put ol 'face 'pl/win-switch-face-background)
    (setq pl/win-switch-face-overlay ol)))

(defun pl/win-switch-off-feedback-func ()
  (delete-overlay pl/win-switch-face-overlay))

(defun pl/win-switch-exit-do-ace-window ()
  (interactive)
  (win-switch-exit)
  (call-interactively 'ace-window))

(defun pl/win-switch-setup-keys ()
  (win-switch-define-key "F" 'pl/toggle-full-window)
  (win-switch-define-key "1" 'delete-other-windows)

  (win-switch-define-key "u" 'winner-undo)
  (win-switch-define-key "r" 'winner-redo)

  (win-switch-define-key "p" 'buf-move-up)
  (win-switch-define-key "n" 'buf-move-down)
  (win-switch-define-key "b" 'buf-move-left)
  (win-switch-define-key "f" 'buf-move-right)
  (win-switch-define-key "\M-o" 'pl/win-switch-exit-do-ace-window)

  (win-switch-set-keys '("k") 'up)
  (win-switch-set-keys '("j") 'down)
  (win-switch-set-keys '("h") 'left)
  (win-switch-set-keys '("l") 'right)

  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window)
  (win-switch-set-keys '("J") 'enlarge-vertically)
  (win-switch-set-keys '("K") 'shrink-vertically)
  (win-switch-set-keys '("H") 'enlarge-horizontally)
  (win-switch-set-keys '("L") 'shrink-horizontally)
  (win-switch-set-keys '(" ") 'other-frame)
  (win-switch-set-keys '("q" [return]) 'exit)
  (win-switch-set-keys '("2") 'split-vertically)
  (win-switch-set-keys '("3") 'split-horizontally)
  (win-switch-set-keys '("0") 'delete-window)
  (win-switch-set-keys '("x") 'delete-window))


(eval-after-load "win-switch"
  '(progn
     (pl/win-switch-setup-keys)
     (setq win-switch-window-threshold 0
           win-switch-idle-time 0.5
           ;; win-switch-other-window-first (lambda () (null (nthcdr 3 (window-list))))
           win-switch-other-window-first nil
           win-switch-provide-visual-feedback t
           win-switch-feedback-background-color "#073642"
           win-switch-feedback-foreground-color "#eeeeee"
           win-switch-other-window-function nil
           win-switch-on-feedback-function nil
           win-switch-off-feedback-function nil
           win-switch-on-feedback-function 'pl/win-switch-on-feedback-func
           win-switch-off-feedback-function 'pl/win-switch-off-feedback-func)

     ;; Wrap around makes things easier
     (win-switch-set-wrap-around +1)))

(defun pl/toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

;;; window operating with hydra
;; http://oremacs.com/2015/01/29/more-hydra-goodness/
(defun pl/hydra-universal-argument (arg)
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))

(defun pl/ace-window-delete ()
  (interactive)
  (ace-window 16))

(require 'hydra nil t)
(with-eval-after-load "hydra"
  (defhydra pl/hydra-window (global-map "C-S-o")
    "window"
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("a" ace-window "ace")
    ("u" pl/hydra-universal-argument "universal")
    ("s" (lambda () (interactive) (ace-window 4)) "swap")
    ("d" pl/ace-window-delete "delete")
    ("x" pl/ace-window-delete "delete")
    ;; o and "RET" will dismiss the Hydra without doing anything.
    ("RET")
    ("o"))

  (key-chord-define-global "jh" 'pl/hydra-window/body))

;;;
(provide '50window)
