;;; 50window.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; (add-to-list 'window-persistent-parameters '(window-side . writable))
;; (add-to-list 'window-persistent-parameters '(window-slot . writable))

(setq switch-to-buffer-preserve-window-point t
      window-min-height 4               ; Let's not have too-tiny windows.
      mouse-autoselect-window nil)
;; (setq fit-window-to-buffer-horizontally t)

;;; Copy from doom-modeline. Fix org-agenda command window height
;; FIXME #183: Force to caculate mode-line height
;; @see https://github.com/seagle0128/doom-modeline/issues/183
(defun tl//modeline-redisplay (&rest _)
  "Call `redisplay' to trigger mode-line height calculations.

Certain functions, including e.g. `fit-window-to-buffer', base
their size calculations on values which are incorrect if the
mode-line has a height different from that of the `default' face
and certain other calculations have not yet taken place for the
window in question.

These calculations can be triggered by calling `redisplay'
explicitly at the appropriate time and this functions purpose
is to make it easier to do so.

This function is like `redisplay' with non-nil FORCE argument.
It accepts an arbitrary number of arguments making it suitable
as a `:before' advice for any function.  If the current buffer
has no mode-line or this function has already been called in it,
then this function does nothing."
  (when mode-line-format
    (redisplay t)))

(advice-add #'fit-window-to-buffer :before #'tl//modeline-redisplay)
(advice-add #'resize-temp-buffer-window :before #'tl//modeline-redisplay)

;; Origial from https://gist.github.com/3402786 An Emacs function to temporarily make
;; one buffer fullscreen. You can quickly restore the old window setup.
(defun tl/toggle-maximize-buffer (&optional buffer-or-name)
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)

      (when buffer-or-name
        (let* ((win (get-buffer-window buffer-or-name)))
          (select-window win)))

      (delete-other-windows))))


;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun tl/rotate-windows-forward (count)
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

(defun tl/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (spacemacs/rotate-windows-forward (* -1 count)))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun tl/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun tl/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun tl/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun tl/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun tl/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))


(defun tl/delete-window (&optional arg)
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
(defun tl/toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(define-key ctl-x-4-map (kbd "C-d") 'tl/toggle-current-window-dedication)

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
(global-set-key (kbd "C-x !") 'tl/toggle-window-split)
(defun tl/toggle-window-split ()
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

(defun tl/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(defun tl/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

;; (global-set-key (kbd "C-x 2") 'tl/vsplit-last-buffer)
;; (global-set-key (kbd "C-x 3") 'tl/hsplit-last-buffer)

;; https://github.com/deb0ch/emacs-winum
(use-package winum
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-ignored-buffers '(" *which-key*"))
    (tl/set-leader-keys
      "`" 'winum-select-window-by-number
      "²" 'winum-select-window-by-number
      ;; "0" 'treemacs-select-window
      "0" 'winum-select-window-0-or-10
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "5" 'winum-select-window-5
      "6" 'winum-select-window-6
      "7" 'winum-select-window-7
      "8" 'winum-select-window-8
      "9" 'winum-select-window-9)
    ;; (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)
    (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
    (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
    (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
    (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
    (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
    (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
    (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
    (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
    (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
    (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
    (winum-mode +1)))

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

;; (setq special-display-function 'tl/display-special-buffer-popup)
;; (add-to-list 'special-display-regexps ".*")  ; match any window

;; (defun tl/display-special-buffer-popup (buffer &optional args)
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
(defun tl/recursive-edit-save-window-config ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (recursive-edit))))

;; RecursiveEditPreservingWindowConfig
;; One can change the window configuration temporarily using RecursiveEdit?.
;; Inspired by a command posted by ErikNaggum in an Emacs Newsgroup, EmilioLopes
;; wrote this macro:

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro tl/recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with \\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `tl/recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; Use it like this:

(global-set-key (kbd "C-c 0") (tl/recursive-edit-preserving-window-config (delete-window)))
(global-set-key (kbd "C-c 2") (tl/recursive-edit-preserving-window-config
                               (split-window-vertically 20)))
(global-set-key (kbd "C-c 3") (tl/recursive-edit-preserving-window-config
                               (split-window-horizontally -52)))
(global-set-key (kbd "C-c 1") (tl/recursive-edit-preserving-window-config
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
(use-package ace-window
  :demand t  ; don'it defer
  :bind (("M-o" . ace-window))
  :commands (ace-window)
  :init
  (progn
    (setq aw-minibuffer-flag t
          aw-select-always t
          aw-dispatch-always t
          aw-ignore-on t
          aw-dispatch-when-more-than 1)
    (setq aw-keys '(?s ?d ?f ?g ?h ?j ?k ?l))

    (tl/set-leader-keys "0" 'dired-sidebar-jump-to-sidebar))
  :config
  (progn
    (defvar tl/aw-mode-line-format " ⧉ %s") ; "♯%s"

    (add-to-list 'aw-ignored-buffers 'dired-sidebar-mode)
    (add-to-list 'aw-ignored-buffers 'org-agenda-mode)

    (add-to-list 'aw-dispatch-alist '(?a tl/jump-to-org-agenda))
    (add-to-list 'aw-dispatch-alist '(?t dired-sidebar-jump-to-sidebar))

    (add-to-list 'aw-dispatch-alist '(?\e keyboard-quit))

    ;; (defun tl//exit-when-escape (char)
    ;;   (when (= char ?\e)
    ;;     (throw 'done 'exit)))
    ;; (advice-add 'aw-dispatch-default :before 'tl//exit-when-escape)
    ;; (advice-remove 'aw-dispatch-default 'tl//exit-when-escape)

    ;; FIXME: conflict with `window-purpose'
    ;; (setq display-buffer-base-action '((display-buffer-reuse-window
    ;;                                     ace-display-buffer)))

    ;; (dolist (l '(("\\*help\\[R" (display-buffer-reuse-mode-window
    ;;                              ace-display-buffer))
    ;;              ("\\*helm"
    ;;               ;; see also: `helm-split-window-default-fn'
    ;;               (display-buffer-pop-up-window))
    ;;              ("magit-diff:" (ace-display-buffer)
    ;;               (inhibit-same-window . t))))
    ;;   (push l display-buffer-alist))

    ;; TODO: set value for ignored buffer like agenda buffer and dired-sidebar
    (defadvice aw-update (around format-ace-window-path activate)
      "add customization for ace window path"
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize
           (format tl/aw-mode-line-format
                   (upcase (apply #'string (reverse path))))
           'face 'aw-mode-line-face)))))
    ;; (ad-deactivate 'aw-update)

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
                      (default-value 'mode-line-format)))))

    (with-eval-after-load 'hydra
      (defhydra hydra-window-size (:color red)
        "Windows size"
        ("h" shrink-window-horizontally "shrink horizontal")
        ("j" shrink-window "shrink vertical")
        ("k" enlarge-window "enlarge vertical")
        ("l" enlarge-window-horizontally "enlarge horizontal"))
      (defhydra hydra-window-frame (:color red)
        "Frame"
        ("f" make-frame "new frame")
        ("x" delete-frame "delete frame"))
      ;; (defhydra hydra-window-scroll (:color red)
      ;;   "Scroll other window"
      ;;   ("n" joe-scroll-other-window "scroll")
      ;;   ("p" joe-scroll-other-window-down "scroll down"))
      (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
      ;; (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
      (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
    (ace-window-display-mode +1)))

;;; golden-ratio.el
;; (require 'golden-ratio nil t)
;; (eval-after-load "golden-ratio"
;;   '(progn
;;      (golden-ratio-mode -1)
;;      (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-select-buffer-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "w3m-mode")
;;      (add-to-list 'golden-ratio-exclude-modes "org-mode")
;;      (add-to-list 'golden-ratio-inhibit-functions 'tl/helm-alive-p)
;;      (require 'ediff nil t)
;;      (if (boundp 'ediff-this-buffer-ediff-sessions)
;;          (add-to-list 'golden-ratio-inhibit-functions 'tl/ediff-comparison-buffer-p))))

(defun tl/ediff-comparison-buffer-p ()
  ediff-this-buffer-ediff-sessions)

(defun tl/helm-alive-p ()
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

(use-package ivy-purpose
  :defer t
  :init
  (progn
    (setq purpose-preferred-prompt 'vanilla)
    (global-set-key [remap purpose-switch-buffer-with-purpose]
                    #'ivy-purpose-switch-buffer-with-purpose)
    (global-set-key [remap purpose-switch-buffer-without-purpose]
                    #'ivy-purpose-switch-buffer-without-purpose)
    (global-set-key [remap purpose-switch-buffer-with-some-purpose]
                    #'ivy-purpose-switch-buffer-with-some-purpose)))

;; https://github.com/bmag/emacs-purpose
(defvar dottl-switch-to-buffer-prefers-purpose nil)
(use-package window-purpose
  ;; :diminish window-purpose
  :defer 3
  :init
  (progn
    (tl/set-leader-keys
      "wpp" #'purpose-x-popwin-close-windows

      "rb" 'purpose-switch-buffer-with-purpose
      "rB" 'switch-buffer-without-purpose
      "rd" 'purpose-toggle-window-purpose-dedicated
      "rD" 'purpose-delete-non-dedicated-windows
      "rp" 'purpose-switch-buffer-with-some-purpose
      "rP" 'purpose-set-window-purpose))
  :config
  (progn
    (purpose-mode +1)

    ;; purpose-user-mode-purposes: recognize purpose according to major mode
    ;; purpose-user-mode-purposes: recognize purpose according to buffer name (for exact names)
    ;; purpose-user-regexp-purposes: recognize purpose according to buffer name (for name patterns)
    (add-to-list 'purpose-user-mode-purposes '(help-mode . popup))
    (add-to-list 'purpose-user-mode-purposes '(helpful-mode . popup))
    (add-to-list 'purpose-user-mode-purposes '(rg-mode . popup))
    (setq purpose-use-default-configuration t)
    (purpose-compile-user-configuration)

    ;; change `switch-to-buffer' display preferences according to
    ;; `dottl-switch-to-buffer-prefers-purpose'. This affects actions
    ;; like `tl/alternate-buffer', and opening buffers from Dired
    (setcdr (assq 'switch-to-buffer purpose-action-sequences)
            (if dottl-switch-to-buffer-prefers-purpose
                '(purpose-display-reuse-window-buffer
                  purpose-display-reuse-window-purpose
                  purpose-display-maybe-same-window
                  purpose-display-maybe-other-window
                  purpose-display-maybe-other-frame
                  purpose-display-maybe-pop-up-window
                  purpose-display-maybe-pop-up-frame)
              '(purpose-display-maybe-same-window
                purpose-display-reuse-window-buffer
                purpose-display-reuse-window-purpose
                purpose-display-maybe-other-window
                purpose-display-maybe-other-frame
                purpose-display-maybe-pop-up-window
                purpose-display-maybe-pop-up-frame)))
    ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
    ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings.
    (setcdr purpose-mode-map nil)

    (use-package window-purpose-x
      :init
      (progn
        (setq purpose-x-popwin-position 'bottom
              ;; if `compilation-window-height' are set, the
              ;; `purpose-x-popwin-height' will be ignore
              purpose-x-popwin-height 0.66
              purpose-x-popwin-width 0.45))
      :config
      (progn
        (add-to-list 'purpose-x-popwin-major-modes 'helpful-mode)
        (add-to-list 'purpose-x-popwin-major-modes 'org-anki-mode)
        ;; (purpose-x-popupify-purpose 'search #'purpose-display-at-right)
        ;; Activate `popwin' emulation.
        (purpose-x-popwin-setup)
        (purpose-x-kill-setup)))))

;; (use-package dimmer
;;   :init
;;   (progn
;;     (setq dimmer-fraction 0.2))
;;   :config
;;   (progn
;;     (dimmer-mode +1)))

(defun tl/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


;;; toggle full window
(defun tl/toggle-full-window()
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
   (kbd "s-o")
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
     ("f" tl/toggle-full-window "full-window")
     ("F" toggle-frame-maximized "frame max")
     ("M-f" toggle-frame-fullscreen "frame fullscreen")
     ("<escape>" nil "cancel")
     ("q" nil "cancel"))))


(use-package eyebrowse
  :config
  (setq eyebrowse-wrap-around t)
  (defhydra help/hydra-left-side/eyebrowse (:color blue :hint nil)
    "
current eyebrowse slot: %(eyebrowse--get 'current-slot)

 _j_ previous _k_ last _l_ next _u_ close _i_ choose _o_ rename _q_ quit
   _a_ 00 _s_ 01 _d_ 02 _f_ 03 _g_ 04 _z_ 05 _x_ 06 _c_ 07 _v_ 08 _b_ 09
"
    ("j" #'eyebrowse-prev-window-config :exit nil)
    ("k" #'eyebrowse-last-window-config)
    ("l" #'eyebrowse-next-window-config :exit nil)
    ("u" #'eyebrowse-close-window-config :exit nil)
    ("i" #'eyebrowse-switch-to-window-config)
    ("o" #'eyebrowse-rename-window-config :exit nil)
    ("q" nil)
    ("a" #'eyebrowse-switch-to-window-config-0)
    ("s" #'eyebrowse-switch-to-window-config-1)
    ("d" #'eyebrowse-switch-to-window-config-2)
    ("f" #'eyebrowse-switch-to-window-config-3)
    ("g" #'eyebrowse-switch-to-window-config-4)
    ("z" #'eyebrowse-switch-to-window-config-5)
    ("x" #'eyebrowse-switch-to-window-config-6)
    ("c" #'eyebrowse-switch-to-window-config-7)
    ("v" #'eyebrowse-switch-to-window-config-8)
    ("b" #'eyebrowse-switch-to-window-config-9))
  (tl/set-leader-keys "l" 'help/hydra-left-side/eyebrowse/body)
  (eyebrowse-mode +1))


;;;
(provide '50window)
