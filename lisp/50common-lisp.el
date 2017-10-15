;;; 50common-lisp.el ---

;;; http://common-lisp.net/project/clbuild/
;; $ clbuild slime-configuration
;; (add-to-list 'load-path "~/opt/clbuild2/source/slime/")
;; (add-to-list 'load-path "~/opt/clbuild2/source/slime/contrib")
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime")
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/contrib")
(require 'slime-autoloads)

(use-package slime
  :defer t
  :init
  (progn
    ;;  contribs
    ;; slime-js conflict with swank-fack see more https://github.com/swank-js/swank-js/issues/40
    ;; (slime-setup '(slime-fancy slime-asdf slime-tramp slime-js slime-repl slime-autodoc))
    ;; (slime-setup '(slime-asdf slime-tramp slime-js slime-repl slime-fuzzy))
    ;; When SLIME is loaded it loads the contribs you set up before in slime-contribs. You can use the command slime-setup
    ;; to reload contribs.
    (setq slime-contribs '(slime-asdf slime-tramp slime-js slime-repl slime-fuzzy))
    (setq inferior-lisp-program "~/opt/clbuild2/clbuild lisp"
          slime-protocol-version 'ignore ; remove annoying warning
          slime-complete-symbol*-fancy t
          ;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          slime-net-coding-system 'utf-8-unix
          slime-default-lisp 'clozure
          slime-enable-evaluate-in-emacs nil
          slime-log-events t
          slime-outline-mode-in-events-buffer nil ; means use outline-mode in *slime-events*
          slime-autodoc-use-multiline-p t
          slime-use-autodoc-mode t
          slime-highlight-compiler-notes t
          slime-fuzzy-completion-in-place nil))
  :config
  (progn
    (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
    (define-key slime-repl-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
    ;; (define-key slime-mode-map (kbd "C-c I") 'slime-inspect)

    ;; slime-lisp-implementations
    ;; You start up your lisps using M-- M-x slime. It will ask you which Lisp to start up, and you use
    ;; the name you defined in slime-lisp-implementations.

    ;; You can switch the "active" REPL using the command C-c C-x c. For more info, see the Slime
    ;; Documentation on controlling multiple connections
    ;; (http://common-lisp.net/project/slime/doc/html/Multiple-connections.html#Multiple-connections).
    (mapcar (lambda (lst) (add-to-list 'slime-lisp-implementations lst))
            '((sbcl ("~/opt/clbuild2/clbuild" "--implementation" "sbcl" "lisp") :coding-system utf-8-unix)
              (sbcl.core ("sbcl" "--core" "sbcl.core-with-swank")
                         :init (lambda (port-file _)
                                 (format
                                  "(swank:start-server %S :coding-system \"utf-8-unix\")\n"
                                  port-file))
                         :coding-system utf-8-unix)
              (cmucl ("lisp"))
              (clozure ("/usr/local/bin/ccl")) ; "/usr/local/bin/ccl64 -K utf-8"
              (ecl ("ecl"))
              (allegro ("/usr/local/stow/AllegroCL/alisp"))
              (clisp ("clisp") :coding-system utf-8-unix)
              (lispworks (""))
              (openmcl ("dx86cl64"))))))

(defun paloryemacs/lisp-mode-init ()
  ;; (imenu-add-to-menubar "Symbols")
  ;; (make-local-variable 'outline-regexp)
  ;; (setq outline-regexp "^(.*")
  ;; setup {} and [] to be treated like ()
  (define-key lisp-mode-map [tab] 'lisp-indent-or-complete)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

  (slime-mode t))

(add-hook 'lisp-mode-hook 'paloryemacs/lisp-mode-init)

;;; this prevents us from requiring the user get dev-lisp/hyperspec (which is non-free) as a hard dependency
(if (file-exists-p "/usr/share/doc/hyperspec")
    (setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/HyperSpec/")
  (setq common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/"))

(defmacro defslime-start (name mapping)
  `(defun ,name ()
     (interactive)
     (let ((slime-default-lisp ,mapping))
       (slime))))

(defslime-start sbcl 'sbcl)
(defslime-start clozure 'clozure)
(defslime-start clisp 'clisp)
(defslime-start cmucl 'cmucl)


;;; ansicl
;; Look up stuff in the ANSI Common Lisp Standard.
;; downloading instructions: (find-file "~/e/cl.e")
;; To use `C-h S' (`info-lookup-symbol') to look up the symbol at
;; point in the manual, add the following

;;; info
(use-package info-look
  :config
  (progn
    (info-lookup-add-help
     :mode 'lisp-mode
     :regexp "[^][()'\" \t\n]+"
     :ignore-case t
     :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))))


;;; Fontify *SLIME Description* buffer for SBCL
(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (let ((buffer "*SLIME Description <sbcl>*"))
    (if (get-buffer "50cl.el")
        (with-current-buffer buffer
          (highlight-regexp
           (concat
            ;; "^Function:\\|"
            ;; "^Macro-function:\\|"
            ;; "^Its associated name.+?) is\\|"
            ;; "^The .+'s arguments are:\\|"
            ;; "^Function documentation:$\\|"
            ;; "^On.+it was compiled from:$"
            ;; "^Its.+\\(is\\|are\\):\\|"
            "^ \\{2\\}Value:\\|"
            "^ \\{2\\}Lambda-list:\\|"
            "^ \\{2\\}Source file:\\|"
            "^ \\{2\\}Source form:$\\|"
            "^ \\{2\\}Derived type:\\|"
            "^ \\{2\\}Class precedence-list:\\|"
            "^ \\{2\\}Direct superclasses:\\|"
            "^ \\{2\\}Direct subclasses:\\|"
            "^ \\{2\\}Direct slots:$\\|"
            "^ \\{6\\}Initargs:\\|"
            "^ \\{6\\}Readers:\\|"
            "^ \\{6\\}Writers:\\|"
            "^ \\{6\\}Documentation:\\|"
            "^ \\{2\\}Documentation:$")
           'hi-green-b)))))

(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
  (slime-description-fontify))


;;----------------------------------------------------------------------------------------------------
(defun paloryemacs/lispdoc (arg)
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

(define-key help-map (kbd "l") 'paloryemacs/lispdoc) ; was view-lossage
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

;;;
(defun lisp-indent-or-complete (&optional arg)
  (interactive "p")
  (if (or (looking-back "^\\s-*") (bolp))
      (call-interactively 'lisp-indent-line)
    (call-interactively 'slime-indent-and-complete-symbol)))


;;; stumpwm
(defun stumpwm ()
  (interactive)
  (slime-connect "127.0.0.1" 4005))

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

;;; redshank
;; (require 'redshank-loader nil t)
;; (eval-after-load "redshank-loader"
;;   `(redshank-setup '(lisp-mode-hook
;;                      slime-repl-mode-hook) t))


;;; Making slime connect to your lisp automatically when you open a lisp file.
;; (defun paloryemacs/start-slime ()
;;   (interactive)
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))
;; (add-hook 'slime-mode-hook 'paloryemacs/start-slime)


(provide '50common-lisp)
;;; 50common-lisp.el ends here
