;;; 50system-trash.el ---

;; Copyright (C) 2009  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;; Using the freedesktop.org trash specification

;; The following code will configure Emacs to use the freedesktop.org trash specification, which is
;; compatible with GNOME and KDE. This means that files deleted in Emacs will show up in the
;; GNOME/KDE trash folder.


;; Note that the following code requires the command `gvfs-trash,’ included with GNOME. If you do
;; not use GNOME, you can install trash-cli and replace ‘ gvfs-trash ’ with `trash.’


;; Use the system trash, except for temp files and stuff
(setq delete-by-moving-to-trash t)

(defcustom system-trash-exclude-names
  '()
  "List of file names of buffers to exclude from system trash."
  :type '(repeat string)
  :group 'trash)

(defcustom system-trash-exclude-matches
  '()
  "List of regexps or functions matching file names to exclude from system trash."
  :type '(repeat (choice regexp function))
  :group 'trash)

(defun call-process-discard-output (program &rest args)
  "Execute program with args without saving any output.
In particular, no temp files are created."
  (eval (append `(call-process ,program nil nil nil) args)))

(defun file-excluded-from-system-trash-p (name)
  "Returns non-nil if file name is excluded from trash."
  (let ((name (file-name-nondirectory name)))
    (or
     (some (lambda (protected-name)
             (string= protected-name name))
           system-trash-exclude-names)
     (some (lambda (protected-match)
             (cond
               ((stringp protected-match) (string-match protected-match name))
               ((functionp protected-match) (funcall protected-match name))
               (t nil)))
           system-trash-exclude-matches))))

(defun trash-or-rm (filename)
  "Attempt to move a file to the trash. If this fails, simply delete it."
  (ignore-errors
    (call-process-discard-output "trash-put" filename))
  (when (file-exists-p filename)
    (call-process-discard-output "rm" "-rf" filename)))

(defun system-move-file-to-trash (filename)
  (if (file-excluded-from-system-trash-p filename)
      (call-process-discard-output "rm" "-rf" filename)
      (trash-or-rm filename)))


(provide 'system-trash)
