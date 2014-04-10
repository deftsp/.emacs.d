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

(global-set-key (kbd "M-D") 'pl/toggle-current-window-dedication)

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



;;; switch window
(eval-after-load "switch-window"
  '(setq switch-window-shortcut-style 'alphabet))

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

(require 'win-switch nil t)

(defun pl/win-switch-setup-keys (&rest dispatch-keys)
  (interactive)
  (win-switch-set-keys '("p") 'up)
  (win-switch-set-keys '("n") 'down)
  (win-switch-set-keys '("b") 'left)
  (win-switch-set-keys '("f") 'right)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window)
  (win-switch-set-keys '("j") 'enlarge-vertically)
  (win-switch-set-keys '("k") 'shrink-vertically)
  (win-switch-set-keys '("h") 'enlarge-horizontally)
  (win-switch-set-keys '("l") 'shrink-horizontally)
  (win-switch-set-keys '(" ") 'other-frame)
  (win-switch-set-keys '("u" [return]) 'exit)
  (win-switch-set-keys '(";") 'split-horizontally)
  (win-switch-set-keys '("-") 'split-horizontally)
  (win-switch-set-keys '("v") 'split-vertically)
  (win-switch-set-keys '("|") 'split-vertically)
  (win-switch-set-keys '("0") 'delete-window)
  (win-switch-set-keys '("x") 'delete-window)
  (dolist (key dispatch-keys)
    (global-set-key key 'win-switch-dispatch)))

(eval-after-load "win-switch"
  '(progn
     (setq win-switch-idle-time 0.75)
     (pl/win-switch-setup-keys (kbd "M-o M-o"))
     (setq win-switch-window-threshold 1)
     (setq win-switch-other-window-first (lambda () (null (nthcdr 3 (window-list)))))
     (setq win-switch-provide-visual-feedback t)
     (setq win-switch-feedback-background-color "#4a708b")
     (setq win-switch-feedback-foreground-color "#eeeeee")
     ;; No special functions, though icicles remaps other-window
     ;; which gets used here and whose argument is respected
     (setq win-switch-on-feedback-function nil)
     (setq win-switch-off-feedback-function nil)
     (setq win-switch-other-window-function nil)
     ;; Wrap around makes things easier
     (win-switch-set-wrap-around 1)))

(defun pl/toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

;;;
(provide '50window)
