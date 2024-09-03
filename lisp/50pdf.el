;;; 50pdf.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-unicode-ligther nil)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)

  (setq pdf-view-resize-factor 1.12)

  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  (add-hook 'pdf-view-mode-hook 'tl/pdf-view-mode-init)
  (add-hook 'pdf-view-mode-hook 'tl/pdf-cleanup-windows-h)
  (add-hook 'pdf-outline-buffer-mode-hook 'tl/pdf-outline-buffer-init)

  ;; solarized dark + bright front
  ;; if change `pdf-view-midnight-colors',  re-open the pdf file to take effect
  (setq pdf-view-midnight-colors '("#d3e4e6" . "#002b36")) ; #839496

  (general-evil-define-key 'normal pdf-view-mode-map
    :prefix ","

    "0"  'image-bol
    "$"  'image-eol

    "d" 'pdf-view-scroll-up-or-next-page
    "e" 'pdf-view-scroll-down-or-previous-page

    ;; Slicing image
    "sm" 'pdf-view-set-slice-using-mouse
    "sb" 'pdf-view-set-slice-from-bounding-box
    "sr" 'pdf-view-reset-slice

    ;; Annotations
    "aD" 	'pdf-annot-delete
    "at" 	'pdf-annot-attachment-dired
    "al" 	'pdf-annot-list-annotations

    ;; Fit image to window
    "fw"    'pdf-view-fit-width-to-window
    "fh"    'pdf-view-fit-height-to-window
    "fp"    'pdf-view-fit-page-to-window

    ;; "q" 'kill-current-buffer
    "gl" 'pdf-view-goto-label
    "gt" 'pdf-view-goto-page


    ;; Slicing image
    "sm" 'pdf-view-set-slice-using-mouse
    "sb" 'pdf-view-set-slice-from-bounding-box
    "sr" 'pdf-view-reset-slice

    ;; Other
    "ss"    'pdf-occur
    "p"     'pdf-misc-print-document
    "o"     'pdf-links-action-perform
    "O"     'pdf-outline
    "n"     'pdf-view-midnight-minor-mode)

  (general-evil-define-key 'normal pdf-view-mode-map
    "d" 'pdf-view-scroll-up-or-next-page
    "e" 'pdf-view-scroll-down-or-previous-page)

  (general-evil-define-key 'visual pdf-view-mode-map
    "ah" 	'pdf-annot-add-highlight-markup-annotation
    "am" 	'pdf-annot-add-markup-annotation
    "ao" 	'pdf-annot-add-strikeout-markup-annotation
    "as" 	'pdf-annot-add-squiggly-markup-annotation
    "at" 	'pdf-annot-add-text-annotation
    "au" 	'pdf-annot-add-underline-markup-annotation

    "y"     'pdf-view-kill-ring-save))


(defun tl/pdf-view-mode-init ()
  (set (make-local-variable 'evil-normal-state-cursor) (list nil))
  (pdf-view-midnight-minor-mode +1)

  (when (fboundp 'turn-off-evil-snipe-override-mode)
    (turn-off-evil-snipe-override-mode))

  (when (fboundp 'turn-off-evil-snipe-mode)
    (turn-off-evil-snipe-mode))

  (display-line-numbers-mode -1))

(defun tl/pdf-outline-buffer-init ()
  (display-line-numbers-mode -1))



;; adapted from doom-emacs
(defun tl/pdf-cleanup-windows-h ()
  "Kill left-over annotation buffers when the document is killed."
  (when (buffer-live-p (bound-and-true-p pdf-annot-list-document-buffer) )
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (buffer-live-p (bound-and-true-p pdf-annot-list-buffer))
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

;; NOTE: use counsel-bookmark to open the bookmarked pdf will jump to bookmarked page. Using counsel-recentf to open the
;; page before quit instead
(use-package saveplace-pdf-view
  :preface
  (require 'bookmark)
  :after pdf-view)

(provide '50pdf)
