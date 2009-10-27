;;; -*- mode: Emacs-Lisp -*-

;; http://common-lisp.net/project/clbuild/
;; $ clbuild slime-configuration
(add-to-list 'load-path "~/src/clbuild/source/slime/")
(add-to-list 'load-path "~/src/clbuild/source/slime/contrib")

(setq slime-backend "~/src/clbuild/.swank-loader.lisp")

(require 'slime)
;; (require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf slime-tramp))
(slime-require :swank-listener-hooks)
;; the rest of SLIME should be loaded automatically when one of the commands `M-x slime' or `M-x slime-connect' is
;; executed the first time.
;; (slime-setup '(slime-autodoc))
;; (when (slime-connected-p)
;;   (slime-eval-async '(swank:swank-require :swank-arglists)))

(defun my-lisp-mode-hook ()
  (slime-mode t)
  ;; (ignore-errors (semantic-default-elisp-setup))
  (imenu-add-to-menubar "Symbols")
  ;; (make-local-variable 'outline-regexp)
  ;; (setq outline-regexp "^(.*")
  (outline-minor-mode))


(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)


;; Making slime connect to your lisp automatically when you open a lisp file.
(defun start-slime ()
  (interactive)
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; (add-hook 'slime-mode-hook 'start-slime)

;; this prevents us from requiring the user get dev-lisp/hyperspec (which is non-free) as a hard dependency
(if (file-exists-p "/usr/share/doc/hyperspec")
    (setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/HyperSpec/")
    (setq common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/"))

(setq inferior-lisp-program "~/src/clbuild/clbuild lisp" ; "/usr/bin/sbcl --noinform"
      lisp-indent-function 'common-lisp-indent-function ;lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-net-coding-system 'utf-8-unix
      slime-startup-animation t
      slime-default-lisp 'sbcl
      slime-enable-evaluate-in-emacs nil
      slime-log-events t
      slime-outline-mode-in-events-buffer nil
      ;;slime-repl-return-behaviour :send-only-if-after-complete
      slime-autodoc-use-multiline-p t
      slime-use-autodoc-mode t
      slime-highlight-compiler-notes t
      slime-fuzzy-completion-in-place nil)

;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "sbcl.core-with-swank")
;;          :init (lambda (port-file _)
;;                  (format
;;                   "(swank:start-server %S :coding-system \"utf-8-unix\")\n"
;;                   port-file))
;;          :coding-system utf-8-unix)
;;         (cmucl ("lisp"))
;;         (ecl ("ecl"))
;;         (allegro ("/usr/local/stow/AllegroCL/alisp"))
;;         (clisp ("clisp") :coding-system utf-8-unix)
;;         (lispworks (""))
;;         (openmcl ("dx86cl64"))))


(defmacro defslime-start (name lisp)
  `(defun ,name ()
     (interactive)
     (slime-start :program ,lisp)))

;; (defslime-start openmcl "/Users/mb/bin/openmcl")
(defslime-start sbcl "/usr/bin/sbcl")
(defslime-start clisp "/usr/bin/clisp")
(defslime-start cmucl "/usr/bin/cmucl")

;;(setf slime-save-buffers nil)
;; (require 'parenface)

;; (defvar slime-auto-compile-timer nil)
;; (defun slime-enable-auto-compile ()
;;   (setf slime-auto-compile-timer
;;         (run-with-idle-timer 3 t `(lambda ()
;;                                     (when (and slime-mode
;;                                                (slime-sexp-at-point)
;;                                                (slime-connected-p))
;;                                       (slime-compile-defun))))))

;; (defun slime-disable-auto-compile ()
;;   (cancel-timer slime-auto-compile-timer))

;; (setf slime-display-edit-hilights nil)

;; (setf slime-filename-translations
;;       (list
;;        (slime-create-filename-translator :machine-instance "soren"
;;                                          :remote-host "80.68.86.18"
;;                                          :username "animaliter")
;;        (slime-create-filename-translator :machine-instance "mail"
;;                                          :remote-host "85.88.193.69"
;;                                          :username "marvin")
;;        (slime-create-filename-translator :machine-instance "deng-mbari"
;;                                          :remote-host "deng-mbari"
;;                                          :username "mbaringer")
;;        (slime-create-filename-translator :machine-instance "debian3164lamp"
;;                                          :remote-host "talkisaction.com"
;;                                          :username "talk")
;;        (list ".*" 'identity 'identity)))

;; ;; setup {} and [] to be treated like ()

;; (modify-syntax-entry ?{ "(}" lisp-mode-syntax-table)
;; (modify-syntax-entry ?} "){" lisp-mode-syntax-table)
;; (modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
;; (modify-syntax-entry ?] ")[" lisp-mode-syntax-table)

;; would love to have < and > match as well, but this totally breaks yaclml :(
;; (defun deng-mbari ()
;;   (interactive)
;;   (slime-connect "127.0.0.1" 5005))

;; (defun mb:disable-font-lock-after-big-change (max-chars)
;;   (when (< max-chars (- (point-max) (point-min)))
;;     (font-lock-mode -1)))

;; (defun mb:slime-narrow-buffer (num-lines)
;;   "Narrow the repl leaving NUM-LINES of output"
;;   (set-marker slime-repl-last-input-start-mark nil)
;;   (let ((inhibit-read-only t))
;;     (narrow-to-region (save-excursion
;;                         (goto-char (slime-repl-input-line-beginning-position))
;;                         (forward-line (- num-lines))
;;                         (point))
;;                       (point-max))))

;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;; ;            (push (lambda (start end changed)
;; ;                    (mb:disable-font-lock-after-big-change 100))
;; ;                  after-change-functions)
;;             (push (lambda () (mb:slime-narrow-buffer 1000))
;;                   post-command-hook)))

;;; ansicl
;;
;; Look up stuff in the ANSI Common Lisp Standard.
;;
;; downloading instructions: (find-file "~/e/cl.e")

;; To use `C-h S' (`info-lookup-symbol') to look up the symbol at
;; point in the manual, add the following

(require 'info-look)

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;;ansi ends there--------------------------------------------------------------------------------------


;; Fontify *SLIME Description* buffer for SBCL

;; (defun slime-description-fontify ()
;;   "Fontify sections of SLIME Description."
;;   (with-current-buffer "*SLIME Description*"
;;     (highlight-regexp
;;      (concat "^Function:\\|"
;;              "^Macro-function:\\|"
;;              "^Its associated name.+?) is\\|"
;;              "^The .+'s arguments are:\\|"
;;              "^Function documentation:$\\|"
;;              "^Its.+\\(is\\|are\\):\\|"
;;              "^On.+it was compiled from:$")
;;      'hi-green-b)))

;; (defadvice slime-show-description (after slime-description-fontify activate)
;;   "Fontify sections of SLIME Description."
;;   (slime-description-fontify))



;;----------------------------------------------------------------------------------------------------
(defun lispdoc (arg)
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the cursor."
  (interactive "P")
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                   "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not symbol-at-point))
        (message "You didn't enter a symbol!")
        (let ((search-type (if arg "full+text+search" "basic+search"))
              (target-symbol (if (string= inp "") default inp)))
          (browse-url (concat "http://lispdoc.com?q=" target-symbol
                              "&search=" search-type))))))

(define-key help-map (kbd "l") 'lispdoc) ; was view-lossage
;;----------------------------------------------------------------------------------------------------

;;; cldoc -- can not work well with slime, use autodoc instead of.
;; (autoload 'turn-on-cldoc-mode "cldoc" nil t)
;; (dolist (hook '(lisp-mode-hook
;;                 slime-repl-mode-hook))
;;   (add-hook hook 'turn-on-cldoc-mode))
;; (add-hook 'slime-repl-mode-hook #'(lambda ()
;;                                     (define-key slime-repl-mode-map " " nil)))
;; (add-hook 'slime-mode-hook #'(lambda ()
;;                                (define-key slime-mode-map " " nil)))

;;; keyboard translate
;; (keyboard-translate ?\( ?\[)
;; (keyboard-translate ?\[ ?\()
;; (keyboard-translate ?\) ?\])
;; (keyboard-translate ?\] ?\))

;;----------------------------------------------------------------------------------------------------
;;paredit-mode
;;----------------------------------------------------------------------------------------------------
;; Reference Table http://mumble.net/~campbell/emacs/paredit/paredit.html
;;(add-to-list 'load-path "~/.emacs.d/paredit/")
;;(require 'paredit)

;; Not sure whether we need this as opposed to a require...
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(eval-after-load 'paredit
  '(progn
    (define-key paredit-mode-map (kbd "H-m l")  'paredit-splice-sexp-killing-backward)
    (define-key paredit-mode-map (kbd "H-m h")  'paredit-splice-sexp-killing-forward)

    (define-key paredit-mode-map (kbd "H-l")   'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "H-M-l") 'paredit-forward-barf-sexp)

    (define-key paredit-mode-map (kbd "H-h")   'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "H-M-h") 'paredit-backward-barf-sexp)))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                inferior-lisp-mode-hook
                inferior-scheme-mode-hook
                slime-repl-mode-hook
                scheme-mode-hook))
  (add-hook hook #'(lambda nil (paredit-mode 1))))

;; (mapc (lambda (mode)
;;  (let ((hook (intern (concat (symbol-name mode)
;;                  "-mode-hook"))))
;;    (add-hook hook (lambda () (paredit-mode +1)))))
;;       '(emacs-lisp lisp inferior-lisp))



;; Apparently this is better idea, because otherwise SLIME will not be a happy bunny.
(eval-after-load 'paredit
  '(progn
    (define-key paredit-mode-map
     (kbd "RET") nil)
    (define-key paredit-mode-map
     (kbd ")") 'paredit-close-parenthesis)
    (define-key paredit-mode-map
     (kbd "M-)") 'paredit-close-parenthesis-and-newline)
    (define-key lisp-mode-shared-map
     (kbd "RET") 'paredit-newline)
    (define-key emacs-lisp-mode-map
     (kbd "RET") 'paredit-newline)))

(eval-after-load 'slime
  '(progn
    ;; (define-key slime-mode-map (kbd "[") 'insert-parentheses)
    ;;(define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
    ;;(define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
    ;;(define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))
    ;;(define-key slime-mode-map (kbd "(") 'paredit-open-parenthesis)
    ;;(define-key slime-mode-map (kbd ")") 'paredit-close-parenthesis)

    ;;(define-key slime-mode-map (kbd "C-M-l") 'paredit-recentre-on-sexp)
    (define-key slime-mode-map (kbd "C-,") 'paredit-backward-slurp-sexp)
    (define-key slime-mode-map (kbd "C-.") 'paredit-forward-slurp-sexp)
    (define-key slime-mode-map (kbd "C-<") 'paredit-backward-barf-sexp)
    (define-key slime-mode-map (kbd "C->") 'paredit-forward-barf-sexp)

    (global-set-key (kbd "C-c S") 'slime-selector)
    ;; (define-key slime-mode-map (kbd "C-c S") 'slime-selector)
    ;; (define-key slime-repl-mode-map (kbd "C-c S") 'slime-selector)
    ;; (define-key sldb-mode-map (kbd "C-c S") 'slime-selector)

    ;; Balanced comments
    (define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
    (define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))



(defun lisp-indent-or-complete (&optional arg)
  (interactive "p")
  (if (or (looking-back "^\\s-*") (bolp))
      (call-interactively 'lisp-indent-line)
      (call-interactively 'slime-indent-and-complete-symbol)))

(eval-after-load "lisp-mode"
  '(progn
    (define-key lisp-mode-map [tab] 'lisp-indent-or-complete)))
;;(define-key slime-mode-map (kbd "C-<return>") 'paredit-newline)
;;;; i hate having to take my key off of ctrl for this and i don't use complete-form anyway...
;;(define-key slime-mode-map (kbd "C-c I") 'slime-inspect)

;; (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
;; (define-key slime-repl-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
(global-set-key (kbd "C-c h") 'slime-hyperspec-lookup)
(global-set-key (kbd "C-c C-z") 'slime-switch-to-output-buffer)


;; (defun insert-balanced-comments (arg)
;;   "Insert a set of balanced comments around the s-expression
;; containing the point.  If this command is invoked repeatedly
;; (without any other command occurring between invocations), the
;; comment progressively moves outward over enclosing expressions."
;;   (interactive "*p")
;;   (save-excursion
;;     (when (eq last-command this-command)
;;       (when (search-backward "#|" nil t)
;;         (save-excursion
;;           (delete-char 2)
;;           (while (and (< (point) (point-max)) (not (looking-at " *|#")))
;;             (forward-sexp))
;;           (replace-match ""))))
;;     (while (> arg 0)
;;       (backward-char 1)
;;       (cond ((looking-at ")") (incf arg))
;;             ((looking-at "(") (decf arg))))
;;     (insert "#|")
;;     (forward-sexp)
;;     (insert "|#")))

;; (defun remove-balanced-comments ()
;;   "Remove a set of balanced comments enclosing point."
;;   (interactive "*")
;;   (save-excursion
;;     (when (search-backward "#|" nil t)
;;       (delete-char 2)
;;       (while (and (< (point) (point-max)) (not (looking-at " *|#")))
;;         (forward-sexp))
;;       (replace-match ""))))

;;; Connect to slime
(defun stumpwm ()
  (interactive)
  (slime-connect "127.0.0.1" 4005))

;;----------------------------------------------------------------------------------------------------
;; expand-trailing-parens & collect-trailing-parens
;;----------------------------------------------------------------------------------------------------
;; These are for people who prefer to have closing parens line up with open parens.
;; That is, not have them all bunched up on one line.

(defun ctp ()
  "collect trailing parens"
  ;; bunches the `dangling parens' in your code up into the useless and unnatural
  ;; position expected by the lisp community.  Depends on font-lock to avoid
  ;; moving a trailing paren onto the end of a comment.
  (interactive)
  (while (re-search-forward "^\\s *)" nil t)
    (end-of-line 0)
    (if (eq 'font-lock-comment-face (get-char-property (1- (point)) 'face))
        (end-of-line 2)
        (delete-indentation -1))
    (beginning-of-line 2)))

;; (defun etp ()
;;   "Expands trailing parens"
;;   ;; This function ignores parens within quotes and comments only if
;;   ;; font-lock is turned on.  If the closing paren is on the same line as
;;   ;; the open then leave it there otherwise give it it's own line.  The
;;   ;; trailing parens won't be lined up in the same column with the opening
;;   ;; paren unless you have my mod for calculate-lisp-indent installed too.
;;   (interactive)
;;   (while (re-search-forward ")" nil t)
;;     (if (not (or (eq 'font-lock-comment-face (get-char-property (1- (point)) 'face))
;;               (eq 'font-lock-string-face (get-char-property (1- (point)) 'face))
;;               (looking-back "^\\s-*)")))
;;         (let ((pos1 (line-beginning-position)))
;;           (if (save-excursion
;;                 (condition-case () (goto-char (scan-sexps (point) -1)) (error nil))
;;                 (eq pos1 (line-beginning-position)))
;;               ()                         ;do nothing if unbalanced or open on same line
;;               (backward-char)
;;               (newline-and-indent t)
;;               (goto-char pos1))))))

;;--------------------------------------------------------------------------------

;; this is a patch to calculate-lisp-indent that puts parens on the 'correct' line.

;; Point is at the point to indent under unless we are inside a string.
;; Call indentation hook except when overridden by lisp-indent-offset
;; or if the desired indentation has already been computed.
;; (let ((normal-indent (current-column))
;;       ;; RGB fixed it to work like I want
;;       (lisp-indent-offset
;;        (if (save-excursion (goto-char indent-point)
;;                            (beginning-of-line)
;;                            (looking-at "\\s-*\\s)"))
;;            0
;;            lisp-indent-offset)))
;;   ;; end of RGB fix
;;   (cond ((elt state 3)

;;-------------------------------------------------------------------------------------------
;;;; counting non-comment lines of code in lisp
;;-------------------------------------------------------------------------------------------
(defun count-loc-lisp ()
  (interactive)
  (let ((lines 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (cond
          ;; skip blank lines
          ((looking-at "^\s*$") nil)
          ;; handle lines with comments
          ((text-property-any (point) (point-at-eol)
                              'face font-lock-comment-delimiter-face)
           ;; is there code on this line too?
           (when (looking-at "\s*\w;")
             ;; yes. count it
             (incf lines)))
          ;; it's not blank and has no comment. count it as code.
          (t (incf lines)))
        (next-line)))
    (message "%S" lines)
    lines))

;; (count-loc-lisp)

;; (defun stump-autocompile nil "compile itself if ~/.stumpwmrc"
;;        (interactive)
;;        (require 'bytecomp)
;;        (if
;;      (string= (buffer-file-name) (expand-file-name (concat
;;                                                     default-directory ".stumpwmrc")))
;;      (slime-compile-file (buffer-file-name))))
;; (add-hook 'after-save-hook 'stump-autocompile)

(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
  the current buffer."
  (interactive)
  ;; (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    ;; (insert ";;;; " file "\n")
    (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
    (insert "(in-package #:" package ")\n\n")))
