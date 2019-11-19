;; ;;; some config about icicle

;; ;; Copyright (C) 2008  Shihpin Tseng

;; ;; Author: Shihpin Tseng <deftsp@gmail.com>


(add-to-list 'load-path "~/.emacs.d/site-lisp/icicles/")
(require 'icicles)

(setq icicle-show-Completions-initially-flag nil
      icicle-command-abbrev-match-all-parts-flag nil)

(icy-mode 1)


;; ;; (icicle-toggle-remote-file-testing)
;; (setq icicle-test-for-remote-files-flag nil)

;; (autoload 'icicle-download-wizard' "icicles-install"
;;   "Run the interactive wizard for downloading Icicles libraries." t)
;; ;; set icicles's download directory, run 'icicle-download-wizard' can auto update
;; (setq icicle-download-dir "~/.emacs.d/packages/icicles/")



;; ;; Historical first: I think it should be the default.
;; (setq icicle-sort-function 'icicle-historical-alphabetic-p
;;       icicle-alternative-sort-function 'icicle-case-string-less-p)

;; ;;; Global Filters
;; ;; read more: http://www.emacswiki.org/cgi-bin/wiki/Icicles_-_Global_Filters
;; ;; buffer-name completion candidates must not match.
;; (setq icicle-buffer-no-match-regexp "^\\ \\|^\\*Ibuffer\\|^\\*Apropos\\|^\\*Backtrace\\|^\\*Completions\\*\\|^\\*Minibuf-1\\*\\|^\\*Help\\|^\\*Bookmark\\|^\\*Messages\\|^\\.newsrc-dribble\\|^\\*Woman-Log\\|^\\*Compilations\\|^\\*Completions\\|^\\*Compile-Log\\|^\\*Calendar\\|^\\*cscope\\|^\\*grep\\|^\\*BBDB\\|^\\*Tree\\|^\\*Group\\|^\\*EMMS Playlist\\|^\\*Summary\\ n\\|Map_Sym.txt\\|^\\#\\|^\\irc.*:\\|localhost:6668")
;; ;; (setq icicle-buffer-no-match-regexp "^\\ ")
;; ;; (setq icicle-buffer-predicate (lambda (bufname)
;; ;;                                 (not (member bufname '(*Completions* *Help*)))))
;; ;; (setq icicle-buffer-extras (list "*scratch*"))



;; ;;; Key bindings
;; (add-hook 'icicle-mode-hook 'bind-my-icicles-keys)

;; ;; Refactored the code in `Icicles - Customizing Key Bindings' at EmacsWiki.
;; (defun bind-my-icicles-keys ()
;;   "Replace some default Icicles bindings with others I prefer."
;;   (when icicle-mode
;;     (dolist (map (append (list minibuffer-local-completion-map
;;                                minibuffer-local-must-match-map)
;;                          (and (boundp 'minibuffer-local-filename-completion-map)
;;                             (list minibuffer-local-filename-completion-map))))
;;       (bind-my-icicles-keys--for-completion-map map)
;;       (bind-my-icicles-keys--for-all-minibuffer-map map))
;;     (let ((map minibuffer-local-map))
;;       (bind-my-icicles-keys--for-all-minibuffer-map map))
;;     (bind-my-icicles-keys--for-icicle-mode-map icicle-mode-map)))

;; (defun bind-my-icicles-keys--for-all-minibuffer-map (map)
;;   (define-key map "\C-e" 'icicle-guess-file-at-point-or-end-of-line)
;;   (define-key map "\C-k" 'icicle-erase-minibuffer-or-kill-line)) ; M-k or C-k
;; ;; I think default icicles key bindings are hard to type.

;; (defun bind-my-icicles-keys--for-completion-map (map)
;;   ;; (define-key map "\C-o" 'icicle-apropos-complete) ; S-Tab
;;   ;; Narrowing is isearch in a sense. C-s in minibuffer is rarely used.
;;   ;; (define-key map "\C-s" 'icicle-narrow-candidates) ; M-*
;;   ;; History search is isearch-backward chronologically:-)
;;   ;; (define-key map "\C-r" 'icicle-history) ; M-h

;;   (define-key map "\M-{" 'icicle-previous-prefix-candidate-action) ; C-up
;;   (define-key map "\M-}" 'icicle-next-prefix-candidate-action)     ; C-down
;;   ;; (define-key map "\C-z" 'icicle-help-on-candidate)                ; C-M-Ret

;;   ;; I do not use icicles' C-v M-C-v anymore.
;;   (define-key map "\C-v" 'scroll-other-window) ; M-C-v and scroll-other-window-downis  C-M-S-v
;;   (define-key map "\M-v" 'scroll-other-window-down))

;; (defun bind-my-icicles-keys--for-icicle-mode-map (map)
;;   ;; These are already bound in global-map. I'll remap them.
;;   (define-key map [f5] nil)               ; icicle-kmacro
;;   (define-key map [f10] nil)
;;   (define-key map [pause] nil))

;; (icicle-remap 'Info-menu       'icicle-Info-menu-cmd       Info-mode-map)

;; ;;; enable icy-mode
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (icicle-mode 1)))

;; ;;; I had used `ffap' for years, and used ffap's guessing feature.
;; (defun icicle-guess-file-at-point ()
;;   "Guess filename at point by the context and insert it."
;;   (interactive)
;;   (require 'ffap-)
;;   (let ((guessed (with-current-buffer icicle-pre-minibuffer-buffer
;;                    (ffap-guesser))))
;;     (when guessed
;;       (icicle-erase-minibuffer)
;;       (insert guessed))))

;; (defun icicle-guess-file-at-point-or-end-of-line ()
;;   "This command inserts default filename if point is at the EOL, Because C-e at the EOL is meaningless,"
;;   (interactive)
;;   (if (eolp)
;;       (icicle-guess-file-at-point))
;;   (end-of-line))

;; ;; (defun icicle-erase-minibuffer-or-kill-line ()
;; ;;   "C-k at the EOL erases whole minibuffer, otherwise do the default."
;; ;;   (interactive)
;; ;;   (if (eolp)
;; ;;       (icicle-erase-minibuffer)
;; ;;       (kill-line)))

;; (defun icicle-erase-minibuffer-or-kill-line ()
;;   "If `C-k' at the EOL erases whole minibuffer and insert  `~/'.
;; If `C-k' at the EOL and the whole minibuffer is `~/', erase whole minibuffer.
;; Or else erases whole minibuffer. "
;;   (interactive)
;;   (if (eolp)
;;       (if (string= (icicle-input-from-minibuffer) "~/")
;;           (icicle-erase-minibuffer)
;;           (icicle-erase-minibuffer)
;;           (icicle-insert-thing "~/" t))
;;       (kill-line)))



;; ;; I feel yucky if pressing C-g is not notified.
;; ;; (defadvice icicle-abort-minibuffer-input (before ding activate)
;; ;;   "Notify when C-g is pressed."
;; ;;   (ding))

;; ;; I do not SELECT other frame.
;; ;; I do not like opening many frames.
;; ;; I do one-on-one by windows.el that is like GnuScreen.
;; ;; (defalias 'icicle-other-window-or-frame 'other-window)

;; ;; same color as transient-mark-mode
;; (setq icicle-region-background "MidnightBlue")

;; ;;  I feel annoying with ding when wrapping candidates.
;; (defadvice icicle-increment-cand-nb+signal-end (around no-ding activate)
;;   "Disable `ding' when wrapping candidates."
;;   (flet ((ding ()))
;;     ad-do-it))

;; (require 'locate)
;; (icicle-define-command icicle-locate    ; Command name
;;                        "Run the program `locate', then visit files.
;; Unlike `icicle-locate-file' this command is a wrapper for the program `locate'." ; Doc string
;;                        find-file                                                 ; Function to perform the action
;;                        "File: " (mapcar #'list (split-string
;;                                                 (shell-command-to-string (format "%s '%s'" locate-command query)) "\n" t))
;;                        nil t nil 'locate-history-list nil nil
;;                        ((query (read-string "Locate: "))))
;; (global-set-key (kbd "C-c f l") 'icicle-locate)
;; (global-set-key (kbd "C-c r f") 'icicle-recent-file)

;; ;;; icicle-locate-file-by-project
;; ;; TODO windows-tab, define-key
;; (defvar project-directory-regexp "^.+/\\(src\\|compile\\)/[^/]+/")
;; (defvar project-file-ignore-regexp "/\\(RCS\\|.svn\\|_darcs\\)/")
;; (defun icicle-locate-file-by-project ()
;;   "Visit a file in current project."
;;   (interactive)
;;   (cond ((string-match project-directory-regexp default-directory)
;;          (let ((default-directory (match-string 0 default-directory))
;;                (icicle-show-Completions-initially-flag t)
;;                (icicle-expand-input-to-common-match-flag nil)
;;                (icicle-must-not-match-regexp project-file-ignore-regexp))
;;            (call-interactively 'icicle-locate-file)))
;;         (t
;;          (error "%s is not a project directory." default-directory))))


;; (icicle-define-command paloryemacs/change-font
;;                        "Change font of current frame."
;;                        (lambda (font)
;;                          (modify-frame-parameters orig-frame
;;                                                   (list (cons 'font font))))
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t nil nil nil nil
;;                        ((orig-frame (selected-frame))
;;                         (orig-font (frame-parameter nil 'font)))
;;                        nil
;;                        (modify-frame-parameters orig-frame
;;                                                 (list (cons 'font orig-font)))
;;                        nil)


(provide '52icicles)
