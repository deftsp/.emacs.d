;;; 50pdf.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (defvar tl--pdf-view-history nil)
  (add-to-list 'savehist-additional-variables 'tl--pdf-view-history)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)




  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)


  (add-hook 'pdf-view-mode-hook 'tl/pdf-view-mode-init)
  (add-hook 'pdf-view-mode-hook 'tl/pdf-cleanup-windows-h)
  (add-hook 'pdf-outline-buffer-mode-hook 'tl/pdf-outline-buffer-init)

  ;; solarized dark + bright front
  (setq pdf-view-midnight-colors '("#a3b4b6" . "#002b36")) ; #839496


  (general-evil-define-key 'normal pdf-view-mode-map
    ;; "q" 'kill-current-buffer
    "d" 'pdf-view-scroll-up-or-next-page
    "e" 'pdf-view-scroll-down-or-previous-page))


(defun tl/pdf-view-mode-init ()
  (set (make-local-variable 'evil-normal-state-cursor) (list nil))
  (set (make-local-variable 'evil-evilified-state-cursor) (list nil))
  (display-line-numbers-mode -1))

(defun tl/pdf-outline-buffer-init ()
  (display-line-numbers-mode -1))



;; adapted from doom-emacs
(defun tl/pdf-cleanup-windows-h ()
  "Kill left-over annotation buffers when the document is killed."
  (when (buffer-live-p pdf-annot-list-document-buffer)
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (buffer-live-p pdf-annot-list-buffer)
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

(add-hook 'pdf-view-change-page-hook 'tl//pdf-remember-page-number-h)
(defun tl//pdf-remember-page-number-h ()
  (when-let (page (and buffer-file-name (pdf-view-current-page)))
    (tl-store-put buffer-file-name page nil "pdf-view")))


(add-hook 'pdf-view-mode-hook 'tl//pdf-restore-page-number-h)

;; Persist current page for PDF files viewed in Emacs
(defvar tl--pdf--page-restored-p nil)
(defun tl//pdf-restore-page-number-h ()
  (when-let (page (and buffer-file-name (tl-store-get buffer-file-name "pdf-view")))
    (and (not tl--pdf--page-restored-p)
         (<= page (or (pdf-cache-number-of-pages) 1))
         (pdf-view-goto-page page)
         (setq-local tl--pdf--page-restored-p t))))


(provide '50pdf)
