;;; 52tex.el ---

;;; auctex

;; install
;; $ ./autogen.sh
;; $ ./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs \
;;  --with-lispdir=/Applications/Emacs.app/Contents/Resources/site-lisp \
;;  --with-texmf-dir=/usr/local/texlive/texmf-local
;; $ make



;; (require 'tex-mik)
;; (eval-after-load "tex"
;;   '(progn
;;     (add-to-list 'TeX-macro-global "~/share/texmf/tex/")
;;     (setq TeX-output-view-style '(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
;;                                   ("^dvi$" "." "xdvi -1 %dS %d")
;;                                   ("^pdf$" "." "start \"\" %o")
;;                                   ("^html?$" "." "start \"\" %o")))))
(setq preview-scale-function 1.6)
;; (setq LaTeX-document-regexp "document\\|CJK\\*?")  ; don't indent in  CJK environment
;; (add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar)
(eval-after-load "tex-mode"
  '(progn
     ;; with Emacs latex mode
     (add-hook 'latex-mode-hook 'turn-on-reftex)))

(eval-after-load "latex"
  '(progn
     (add-hook 'LaTeX-mode-hook #'paloryemacs/init-Tex)     ; with AUCTeX LaTeX mode
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

(defun paloryemacs/init-Tex ()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq-default TeX-master nil)
  (setq TeX-auto-untabify t) ; do not use TAB char
  (setq TqeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t))



;; All that was left was to add a command to convert the DVI files into PDF. AUCTex already ships with a DVI to PS
;; script under Command called FILE (yeah, go figure why they called it that). I simply added the following to my
;; .emacs:
;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;;     '("DVI to PDF" "dvipdfm %d" TeX-run-command t t) t))

;;; make the paper format customizable via the format string %paperformat
;; (eval-after-load 'tex
;;   '(progn
;;      (setq-default TeX-paper-format "a4")
;;      (make-variable-buffer-local 'TeX-paper-format)
;;      (add-to-list 'TeX-expand-list (list "%(paperformat)" 'TeX-paper-format))
;;      (add-to-list 'TeX-expand-list (list "%(xdvipaperformat)" 'TeX-xdvi-paper-format))
;;      (defvar TeX-paper-format "a4")
;;      (add-to-list 'TeX-command-list (list "dviPS" "dvips %d -Ppdf -G0 -j0 -t %(paperformat) -o %f" 'TeX-run-command nil t))))

;; (defun TeX-paper-format ()
;;   (cond ((string= TeX-paper-format "a4r")
;;          "landscape")
;;         (t
;;          TeX-paper-format)))

;; (defun TeX-xdvi-paper-format ()
;;   (cond ((string= TeX-paper-format "landscape")
;;          "a4r")
;;         (t
;;          TeX-paper-format)))


;; This hook will store bibitems when you save a BibTeX buffer.
;; (add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
;(autoload 'BibTeX-auto-store "latex" no-doc t)

;; (autoload 'tex-mode "tex" no-doc t)
;; (autoload 'plain-tex-mode "tex" no-doc t)
;; (autoload 'ams-tex-mode "tex" no-doc t)
;; (autoload 'TeX-auto-generate "tex" no-doc t)
;; (autoload 'TeX-auto-generate-global "tex" no-doc t)
;; (autoload 'TeX-insert-quote "tex" no-doc t)
;; (autoload 'TeX-submit-bug-report "tex" no-doc t)
;; (autoload 'japanese-plain-tex-mode "tex-jp" no-doc t)
;; (autoload 'japanese-latex-mode "tex-jp" no-doc t)
;; (autoload 'japanese-slitex-mode "tex-jp" no-doc t)
;; (autoload 'texinfo-mode "tex-info" no-doc t)
;; (autoload 'latex-mode "latex" no-doc t)
;; (provide 'tex-site) ;taken from tex-site.el - reftex needs this

;; ;; 01.06.2004 fix this eventually...
;; (setq TeX-auto-global "")

;; ;; 08.09.2005 fix again...
;; (setq AUCTeX-version "11.80")
;; (setq AUCTeX-date "11.80")
;; (setq TeX-data-directory TeX-lisp-directory)

;(when (and emacs>=21p (require 'tex-font nil t))
;  (tex-font-setup))
;; (require 'font-latex "font-latex" t) ;better fontification
;;add subdir doc to Info-default-directory-list
;; (setq Info-default-directory-list
;;       (cons (concat (file-name-as-directory TeX-lisp-directory) "doc")
;;             Info-default-directory-list))
;(TeX-command "dviPS landscape" 'TeX-master-file)

;;RefTex
;; (setq reftex-plug-into-AUCTeX t)
;; (require 'reftex "reftex" t)

;; (defun tex-build-command-function (cmd &optional recenter-output-buffer save-buffer override-confirm)
;;   "Build a TeX-command function."
;;    (` (lambda()
;;         (interactive)
;;         (when (, save-buffer) (save-buffer))
;;         (when (, recenter-output-buffer) (TeX-recenter-output-buffer nil))
;;         (TeX-command (, cmd) 'TeX-master-file (if (, override-confirm) 1 -1)))))


;; (define-skeleton tex-insert-dollar
;;   "Inserts $$" nil "$" _ "$")

; (define-skeleton tex-insert-sig
;   "Inserts \\sig{}" nil "\\sig{" _ "}")
;
;; (define-skeleton skeleton-scrartcl
;;    "Inserts a Latex article skeleton into current buffer.
;;  This only makes sense for empty buffers."
;;    nil
;;    "\\documentclass[a4paper]{scrartcl}\n"
;;    "\\usepackage{ngerman}\n"
;;    "\\usepackage[latin1]{inputenc}\n"
;;    "\\begin{document}\n"
;;    _ "\n"
;;    "\\end{document}")

;; (define-skeleton latex-description-textbf
;; "Insert a latex description in bold face"
;; nil "[\\textbf{" _ ":}] ")

;; (setq LaTeX-extra-key-map (make-keymap))

;; (add-hook 'LaTeX-mode-hook 'xsteve-latex-mode-init)

;; (defun xsteve-latex-mode-init()
;;   (interactive)
;;   (define-key LaTeX-mode-map [f6] (tex-build-command-function "LaTeX" nil t))
;;   (define-key LaTeX-mode-map [(shift f6)] (tex-build-command-function "PdfLaTeX" t t))
;;   (define-key LaTeX-mode-map [(super f6)] (tex-build-command-function "dviPS" t))
;;   (when unixp (define-key LaTeX-mode-map [(meta f6)] (tex-build-command-function "dviPS" t)))

;;   (define-key LaTeX-mode-map [(super control f6)] (tex-build-command-function "dviPS landscape" t))
;;   (define-key LaTeX-mode-map [(super meta f6)] (tex-build-command-function "PS2pdf" t))
;;   (define-key LaTeX-mode-map [f7]  'TeX-next-error)
;;   (define-key LaTeX-mode-map [f8] (tex-build-command-function (if win32p "YAP" "View") nil))
;;   (define-key LaTeX-mode-map [(super f8)] (tex-build-command-function "Ghostview" nil))
;;   (define-key LaTeX-mode-map [(super meta f8)] (tex-build-command-function "View PDF" nil))
;;   (define-key LaTeX-mode-map [f9] (lambda () (interactive) (save-buffer) (start-process-shell-command "scons-preview" nil "scons -u preview=1")))
;;   (define-key LaTeX-mode-map [(f10)]  'reftex-toc)
;;   (define-key LaTeX-mode-map [(control tab)]  'TeX-complete-symbol)
;;   (define-key LaTeX-mode-map [(return)]  'newline-and-indent)

;;   (make-local-variable 'skeleton-end-hook)
;;   (setq skeleton-end-hook nil)

;;   (define-key LaTeX-mode-map [(super x)] LaTeX-extra-key-map)
;;   (define-key LaTeX-mode-map [(control ?X)] LaTeX-extra-key-map)
;;   (define-key LaTeX-extra-key-map "u" 'latex-insert-unit-command)
;;   (define-key LaTeX-extra-key-map "t" 'latex-description-textbf)

;;   ;;(define-key LaTeX-mode-map [(super ?4)] (tex-build-embrace-function "$" "$"))
;;   ;;(define-key LaTeX-mode-map [(meta s)] (tex-build-embrace-function "\\sig{" "}"))

;;   ;;(define-key LaTeX-mode-map [(super ?4)] 'tex-insert-dollar)
;;   ;;(define-key LaTeX-mode-map [(meta s)] 'tex-insert-sig)
;;   ;;(define-key LaTeX-mode-map [(meta k)] 'tex-insert-kw)

;;   (auto-fill-mode 1)
;;   (setq fill-column 90)
;;   (setq TeX-open-quote "\"\`")
;;   (setq TeX-close-quote "\"\'")
;;   ;;(setq TeX-open-quote "\"")
;;   ;;(setq TeX-close-quote "\"")
;;   (imenu-add-to-menubar "Structure")

;;   (turn-on-reftex) ; use reftex

;;;TeX-Mode
;; (add-hook 'TeX-mode-hook
;;           '(lambda ()
;;              (setq TeX-open-quote "\"")
;;              (setq TeX-close-quote "\"")))

;;; bibtex
;; (add-hook 'bibtex-mode-hook
;;           '(lambda ()
;;              (define-key bibtex-mode-map (quote [246]) "{\\\"o}");ö
;;              (define-key bibtex-mode-map (quote [228]) "{\\\"a}");ä
;;              (define-key bibtex-mode-map (quote [252]) "{\\\"u}");ü
;;              ))

;;; latex-doc - get help for latex commands, environments, packages
;; (add-site-lisp-load-path "latex-doc/")
;; (when (require 'latex-doc nil t)
;;   (latex-doc-initialize))

(provide '50tex)
