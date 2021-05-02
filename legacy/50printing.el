;;; Printing


(use-package printing
  :defer t
  :init
  (progn
    (setq lpr-command "lpr"
          lpr-switches nil)

    ;; The printer-name and ps-printer-name variables don’t need to be set, as they are implicit set by pr-ps-printer-alist and
    ;; pr-txt-printer-alist.

    (setq pr-faces-p t)

    (setq pr-path-alist '((unix PATH)
                          (cygwin PATH)
                          (windows PATH)))
    (setq pr-txt-printer-alist
          '((prt_06a "lpr" nil "prt_06a")
            (prt_07c nil   nil "prt_07c")))

    (setq pr-ps-name       'lps_06b)
    ;; (setq pr-ps-printer-alist
    ;;       '((lps_06b "lpr" nil "-P" "lps_06b")
    ;;         (lps_07c "lpr" nil nil  "lps_07c")
    ;;         (lps_08c nil   nil nil  "lps_08c")))
    (setq pr-gv-command "gv")
    (setq pr-gs-command "gs"))
  :config
  (progn
    (pr-update-menus t)))


;;; print some faces in bold or italic
;; available faces are: font-lock-comment-face, font-lock-attribute-face, font-lock-value-face, font-lock-type-face,
;; font-lock-keyword-face, font-lock-function-name-face, font-lock-string-face, font-lock-doc-string-face
(use-package ps-print
  :defer t
  :init
  (progn
    ;; (setq ps-italic-faces '(font-lock-comment-face))
    ;; (setq ps-bold-faces '(font-lock-keyword-face font-lock-function-name-face))
    ;; (setq-default ps-underlined-faces nil)


;;; page margins
    ;; (setq ps-left-margin  (/ (* 72  1.5) 2.54)) ;   1.5 cm
    ;; (setq ps-right-margin (/ (* 72  0.7) 2.54)) ;   0.7 cm
    ;; (setq ps-inter-column (/ (* 72  1.0) 2.54)) ;   1 cm
    ;; (setq ps-bottom-margin (/ (* 72  0.5) 2.54)) ; 0.5 cm
    ;; (setq ps-bottom-margin (/ (* 72  0.9) 2.54)) ; 0.9 cm
    ;; (setq ps-top-margin    (/ (* 72  0.8) 2.54)) ; 0.8 cm
    ;; (setq ps-header-offset (/ (* 72  0.3) 2.54)) ; 0.3 cm
    ;; (setq ps-header-line-pad 0.15)
    ;; (setq ps-n-up-margin (/ (* 72  0.5) 2.54)) ; 0.5 cm

;;; *Specify the number of pages per sheet paper.
    ;; (setq ps-n-up-printing 2)

    ;;(ps-line-lengths)
    ;; (setq ps-font-size   '(7 . 8.5))
    ;; (setq ps-font-size   '(5 . 6.5))
    ;; (setq ps-font-size   '(14 . 6.5))
    ;; (setq ps-font-size   14)
    ;; (setq ps-font-size 7)
    ;; (setq ps-header-font-size '(10 . 12))
    ;; (setq ps-header-title-font-size '(12 . 14))

    ;; If you have a duplex-capable printer (one that prints both sides of
    ;; the paper), set ps-spool-duplex to t.  Ps-print will insert blank
    ;; pages to make sure each buffer starts on the correct side of the
    ;; paper.
    ;; (setq ps-spool-duplex t)

    ;; (setq ps-spool-tumble nil) ; binding on left or right of page
    ;; (setq ps-spool-tumble t) ; binding on top or bottom of page
    (setq ps-print-color-p nil)  ; don't use colors for printing
    (setq ps-paper-type 'a4 ) ; the type of paper
    ))

;;; ps-print

;; (setf ps-landscape-mode nil)
;; (setf ps-line-number 'zebra)
;; (setf ps-zebra-stripes t)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf")))

(provide '50printing)
