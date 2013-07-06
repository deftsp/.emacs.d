;;; 50maxima.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


(add-to-list 'load-path "/Applications/Maxima.app/Contents/Resources/maxima/share/maxima/5.28.0/emacs")

(setenv "PATH"
        (concat
         (getenv "PATH")
         "/Applications/Maxima.app/Contents/Resources/maxima/bin:"))

(add-to-list 'exec-path "/Applications/Maxima.app/Contents/Resources/maxima/bin")


(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)


(eval-after-load "imaxima"
  '(progn
    (setq imaxima-use-maxima-mode-flag t)
    (setq imaxima-print-tex-command "latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf") ;"latex %s; dvips -o imax.ps %s; gv imax.ps"
    (setq imaxima-max-scale 0.85)
    (setq imaxima-scale-factor 1.0)
    (setq imaxima-fnt-size "LARGE")     ; small | normalsize | large | Large | LARGE | huge | Huge
    (setq imaxima-pt-size 11)                     ; The type size used in LaTeX: 9 | 10 | 11 | 12
    (setq imaxima-label-color "#d0bf8f")
    (setq imaxima-equation-color "DeepSkyBlue2") ; "#b6d3d6"
    (setq imaxima-linearize-flag t)))

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
