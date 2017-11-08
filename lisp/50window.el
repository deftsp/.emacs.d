;;; 50window.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(setq switch-to-buffer-preserve-window-point t
      window-min-height 4               ; Let's not have too-tiny windows.
      mouse-autoselect-window nil)

;; from https://gist.github.com/3402786
(defun paloryemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun paloryemacs/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun paloryemacs/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (spacemacs/rotate-windows-forward (* -1 count)))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun paloryemacs/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun paloryemacs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

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
(defun paloryemacs/toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(define-key ctl-x-4-map (kbd "C-d") 'paloryemacs/toggle-current-window-dedication)

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
(global-set-key (kbd "C-x !") 'paloryemacs/toggle-window-split)
(defun paloryemacs/toggle-window-split ()
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

(defun paloryemacs/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(defun paloryemacs/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(global-set-key (kbd "C-x 2") 'paloryemacs/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'paloryemacs/hsplit-last-buffer)


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

;; (setq special-display-function 'paloryemacs/display-special-buffer-popup)
;; (add-to-list 'special-display-regexps ".*")  ; match any window

;; (defun paloryemacs/display-special-buffer-popup (buffer &optional args)
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
(defun paloryemacs/recursive-edit-save-window-config ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (recursive-edit))))

;; RecursiveEditPreservingWindowConfig
;; One can change the window configuration temporarily using RecursiveEdit?.
;; Inspired by a command posted by ErikNaggum in an Emacs Newsgroup, EmilioLopes
;; wrote this macro:

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro paloryemacs/recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with \\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `paloryemacs/recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; Use it like this:

(global-set-key (kbd "C-c 0") (paloryemacs/recursive-edit-preserving-window-config (delete-window)))
(global-set-key (kbd "C-c 2") (paloryemacs/recursive-edit-preserving-window-config
                               (split-window-vertically 20)))
(global-set-key (kbd "C-c 3") (paloryemacs/recursive-edit-preserving-window-config
                               (split-window-horizontally -52)))
(global-set-key (kbd "C-c 1") (paloryemacs/recursive-edit-preserving-window-config
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

(require 'ace-window nil t)

(defvar paloryemacs/aw-mode-line-format "♯%s")

(defadvice aw-update (around format-ace-window-path activate)
  "add customization for ace window path"
  (avy-traverse
   (avy-tree (aw-window-list) aw-keys)
   (lambda (path leaf)
     (set-window-parameter
      leaf 'ace-window-path
      (propertize
       (format paloryemacs/aw-mode-line-format
               (upcase (apply #'string (reverse path))))
       'face 'aw-mode-line-face)))))
;; (ad-deactivate 'aw-update)

(with-eval-after-load "ace-window"
  (defadvice ace-window-display-mode (after
                                      do-not-insert-to-mode-line-with-powerline
                                      activate)
    "do not auto insert ace window path into modeline with powerline."
    (when (and (boundp 'powerline-git-state-mark-modeline)
               powerline-git-state-mark-modeline
               ace-window-display-mode)
      (set-default 'mode-line-format
                   (assq-delete-all
                    'ace-window-display-mode
                    (default-value 'mode-line-format))))))

(when (fboundp 'ace-window-display-mode)
  (ace-window-display-mode +1))

;;; golden-ratio.el
;; (require 'golden-ratio nil t)
;; (eval-after-load "golden-ratio"
;;   '(progn
;;      (golden-ratio-mode -1)
;;      (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-select-buffer-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "org-mode")
;;      (add-to-list 'golden-ratio-inhibit-functions 'paloryemacs/helm-alive-p)
;;      (require 'ediff nil t)
;;      (if (boundp 'ediff-this-buffer-ediff-sessions)
;;          (add-to-list 'golden-ratio-inhibit-functions 'paloryemacs/ediff-comparison-buffer-p))))

(defun paloryemacs/ediff-comparison-buffer-p ()
  ediff-this-buffer-ediff-sessions)

(defun paloryemacs/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;;; buffer-move
;; swap buffers without typing C-x b on each window
;; it can be replaced by ace window
(eval-after-load "buffer-move"
  '(progn
     (global-set-key (kbd "M-[ p") 'buf-move-up)
     (global-set-key (kbd "M-[ n") 'buf-move-down)
     (global-set-key (kbd "M-[ b") 'buf-move-left)
     (global-set-key (kbd "M-[ f") 'buf-move-right)))

;;; popwin
;; https://github.com/m2ym/popwin-el
(use-package popwin
  :config
  (progn
    ;; (global-set-key (kbd "C-z") popwin:keymap)
    (popwin-mode +1)
    (paloryemacs/set-leader-keys "wpm" 'popwin:messages)
    (paloryemacs/set-leader-keys "wpp" 'popwin:close-popup-window)

    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config nil)

    ;; buffers that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '(" *undo-tree*"           :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
    (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)))


(defun paloryemacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


;;; toggle full window
(defun paloryemacs/toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

;;; window operating with hydra
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(require 'hydra nil t)
(with-eval-after-load "hydra"
  (require 'windmove) ; for hydra-move-splitter-*
  (global-set-key
   (kbd "M-o")
   (defhydra hydra-window (:color teal)
     "
Move Point^^^^   Move Splitter   ^Ace^                       ^Split^                          Tranpose
--------------------------------------------------------------------------------------------------
_k_, _<up>_      Shift + Move    _a_: ace-window             _2_: split-window-below         _t_: transpose-frame
_h_, _<left>_                    _s_: ace-window-swap        _3_: split-window-right         _e_: rotate-frame-clockwise
_j_, _<down>_                    _d_: ace-window-delete      ^ ^                             _y_: flip-frame vertically
_l_, _<right>_                   ^ ^                         ^ ^                             _c_: flop-frame horizontally
You can use arrow-keys or HJKL.
"
     ("h" windmove-left nil)
     ("j" windmove-down nil)
     ("k" windmove-up nil)
     ("l" windmove-right nil)

     ("<left>" windmove-left nil)
     ("<down>" windmove-down nil)
     ("<up>" windmove-up nil)
     ("<right>" windmove-right nil)

     ("H" hydra-move-splitter-left nil :color red)
     ("J" hydra-move-splitter-down nil :color red)
     ("K" hydra-move-splitter-up nil :color red)
     ("L" hydra-move-splitter-right nil :color red)

     ("<S-left>" hydra-move-splitter-left nil)
     ("<S-down>" hydra-move-splitter-down nil)
     ("<S-up>" hydra-move-splitter-up nil)
     ("<S-right>" hydra-move-splitter-right nil)

     ("=" balance-windows "balance")

     ("3" (lambda ()
            (interactive)
            (split-window-right)
            (windmove-right))
      "vert")
     ("2" (lambda ()
            (interactive)
            (split-window-below)
            (windmove-down))
      "horz")

     ("t" (if (fboundp 'transpose-frame)
              (funcall 'transpose-frame)
            (message "transpose-frame is not defined")) "'")

     ("e" rotate-frame-clockwise nil)
     ("y" flip-frame nil)
     ("c" flop-frame nil)

     ("`" other-frame "`")

     ("1" delete-other-windows "one")

     ("u" hydra-universal-argument "universal arg")
     ("M-u" winner-undo "undo-win")
     ("M-r" winner-redo "redo-win")

     ("a" ace-window "ace")
     ("o" other-window "other window")
     ("M-o" other-window "other window")
     ("s" ace-swap-window "swap")
     ("d" ace-delete-window "del")
     ("x" delete-window)
     ("i" ace-maximize-window "ace-one")
     ;; ("b" ido-switch-buffer "buf")
     ("b" ivy-switch-buffer "buf")
     ("m" bookmark-jump "bmk")
     ("f" paloryemacs/toggle-full-window "full-window")
     ("F" toggle-frame-maximized "frame max")
     ("M-f" toggle-frame-fullscreen "frame fullscreen")
     ("<escape>" nil "cancel")
     ("q" nil "cancel"))))

;;;
(provide '50window)
