;;; 50ocaml.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(eval-after-load "tuareg"
  '(progn
     (define-key tuareg-mode-map (kbd "C-c C-z") 'tuareg-run-ocaml)
     ;;; select repl buffer after display it
     (defadvice tuareg-run-ocaml (after select-repl-window activate)
       (let* ((buf (get-buffer "*ocaml-toplevel*"))
              (wind (get-buffer-window buf)))
         (when wind
           (select-window wind))))))

;; Indent `=' like a standard keyword.
;; (setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
;; (setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
;; (setq tuareg-in-indent 0)

(defun paloryemacs/tuareg-mode-init ()
  (auto-fill-mode 1))

(add-hook 'tuareg-mode-hook (paloryemacs/tuareg-mode-init))

;; flymake
;; (require 'flymake)

;; (defun flymake-ocaml-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-ocaml-cmdline))

;; (defun flymake-get-ocaml-cmdline (source base-dir)
;;   (list "ocaml_flycheck.pl"
;;         (list source base-dir)))

;; (push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)

;; (add-hook 'tuareg-mode-hook
;;  '(lambda ()
;;     (if (not (null buffer-file-name)) (flymake-mode))))




(provide '50ocaml)
;;; 50ocaml.el ends here
