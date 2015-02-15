;;; 50maxima.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(add-to-list 'load-path "/Applications/Maxima.app/Contents/Resources/maxima/share/maxima/5.28.0/emacs")
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


(provide '50maxima)
;;; 50maxima.el ends here
