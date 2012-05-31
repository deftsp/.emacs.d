;;; 50maxima.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


;; maxima include imaxima, there's no need to install app-emacs/imaxima
;; (autoload 'maxima-mode "maxima" "Maxima mode" t)
;; (autoload 'imaxima "imaxima" "Image support for Maxima." t)
;; (autoload 'imath-mode "imath" "Interactive Math minor mode." t)
;; (autoload 'maxima "maxima" "Maxima interaction" t)

(eval-after-load "imaxima"
  '(setq imaxima-use-maxima-mode-flag t
    imaxima-print-tex-command "latex %s; dvips -o imax.ps %s; gv imax.ps"
    imaxima-linearize-flag t
    imaxima-max-scale 0.85
    imaxima-scale-factor 1.0
    imaxima-fnt-size "normalsize"       ; small | normalsize | large | Large | LARGE | huge | Huge
    imaxima-pt-size 10                  ; The type size used in LaTeX: 9 | 10 | 11 | 12
    imaxima-label-color "red"
    imaxima-equation-color "#b6d3d6"))

;; (setq maxima-info-dir "/usr/share/info/"
;;       maxima-use-dynamic-complete t)

;; (defun pl/maxima-mode-hook ()
;;   (setq pl/starting-imaxima nil)
;;   (defun maxima-start ()
;;     "Start the Maxima process."
;;     (interactive)
;;     (if (not (processp inferior-maxima-process))
;;         (if (not pl/starting-imaxima)
;;             (let ((origbuffer (current-buffer)))
;;               (setq pl/starting-imaxima t)
;;               (imaxima)
;;               (setq pl/starting-imaxima nil)
;;               (switch-to-buffer origbuffer))))
;;     (if (processp inferior-maxima-process)
;;         (unless (eq (process-status inferior-maxima-process) 'run)
;;           (delete-process inferior-maxima-process)
;;           (save-excursion
;;             (set-buffer "*maxima*")
;;             (erase-buffer))
;;           (setq inferior-maxima-process nil)))
;;     (unless (processp inferior-maxima-process)
;;       (setq maxima-input-end 0)
;;       (let ((mbuf)
;;             (cmd))
;;         (if maxima-args
;;             (setq cmd
;;                   (append (list 'make-comint "maxima" maxima-command
;;                                 nil) (split-string maxima-args)))
;;             (setq cmd (list 'make-comint "maxima" maxima-command)))
;;         (setq mbuf (eval cmd))
;;         (save-excursion
;;           (set-buffer mbuf)
;;           (setq inferior-maxima-process (get-buffer-process mbuf))
;;           (if maxima-fix-double-prompt
;;               (add-to-list 'comint-output-filter-functions
;;                            'maxima-remove-double-prompt))
;;           (accept-process-output inferior-maxima-process)
;;           (while (not (maxima-new-prompt-p))
;;             (accept-process-output inferior-maxima-process))
;;           (inferior-maxima-mode)))
;;       (sit-for 0 maxima-after-output-wait))))

;; (add-remove-hook 'maxima-mode-hook 'pl/maxima-mode-hook)




(provide '50maxima)


;;; 50maxima.el ends here
