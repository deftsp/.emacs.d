;;; tl-cache.el -*- lexical-binding: t; -*-
;; Adapted from tl-emacs
;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `tl-store-dir'/`tl-store-location').

(defvar tl-store-dir (concat user-emacs-directory "store/")
  "Directory to look for and store data accessed through this API.")

(defvar tl-store-persist-alist '(t)
  "An alist of alists, containing lists of variables for the tl cache library
to persist across Emacs sessions.")

(defvar tl-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`tl-store-dir'). One file may contain multiple cache entries.")

(defvar tl--store-table (make-hash-table :test 'equal))
(defvar tl--inhibit-flush nil)

(defun tl-save-persistent-store-h ()
  "Hook to run when an Emacs session is killed. Saves all persisted variables
listed in `tl-store-persist-alist' to files."
  (let (locations)
    (let ((tl--inhibit-flush t))
      (dolist (alist (butlast tl-store-persist-alist 1))
        (cl-loop with location = (car alist)
                 for var in (cdr alist)
                 do (tl-store-put var (symbol-value var) nil location)
                 and do (cl-pushnew location locations))))
    (mapc #'tl--store-flush locations)))
(add-hook 'kill-emacs-hook #'tl-save-persistent-store-h)


;;
;; Library

;;;###autoload
(defun tl-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits. This cannot persist buffer-local variables."
  (dolist (var variables)
    (when (tl-store-member-p var location)
      (set var (tl-store-get var location))))
  (setf (alist-get location tl-store-persist-alist)
        (append variables (alist-get location tl-store-persist-alist))))

;;;###autoload
(defun tl-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol).
Variables to persist are recorded in `tl-store-persist-alist'. Does not affect
the actual variables themselves or their values."
  (if variables
      (setf (alist-get location tl-store-persist-alist)
            (cl-set-difference (cdr (assq location tl-store-persist-alist))
                               variables))
    (delq! location tl-store-persist-alist 'assoc)))

(defun tl--store-init (location)
  (or (gethash location tl--store-table)
      (let* ((file-name-handler-alist nil)
             (location-path (expand-file-name location tl-store-dir)))
        (if (file-exists-p location-path)
            (puthash location
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (setq buffer-file-coding-system 'binary)
                       (insert-file-contents-literally location-path)
                       (read (current-buffer)))
                     tl--store-table)
          (puthash location (make-hash-table :test 'equal)
                   tl--store-table)))))

(defun tl--store-get (key location &optional default-value)
  (let* ((location-data (tl--store-init location))
         (data (gethash key location-data default-value)))
    (if (and (not (eq data default-value))
             (or (null (car data))
                 (not (time-less-p (car data) (current-time)))))
        (cdr data)
      default-value)))

(defun tl--store-put (key value location &optional ttl)
  (puthash key (cons (if ttl (time-add (current-time) ttl)) value)
           (tl--store-init location))
  (tl--store-flush location))

(defun tl--store-flush (location)
  (unless tl--inhibit-flush
    (let ((file-name-handler-alist nil)
          (coding-system-for-write 'binary)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil)
          (data (tl--store-init location)))
      (make-directory tl-store-dir 'parents)
      (with-temp-file (expand-file-name location tl-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun tl-store-get (key &optional location default-value)
  "Retrieve KEY from LOCATION (defaults to `tl-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (tl--store-get key (or location tl-store-location) default-value))

;;;###autoload
(defun tl-store-put (key value &optional ttl location)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL is the time (in
seconds) until this cache entry expires. LOCATION is the super-key to store this
cache item under. It defaults to `tl-store-location'."
  (tl--store-put key value (or location tl-store-location) ttl))

;;;###autoload
(defun tl-store-rem (key &optional location)
  "Clear a cache LOCATION (defaults to `tl-store-location')."
  (let ((location (or location tl-store-location)))
    (remhash key (tl--store-init location))
    (let ((table (tl--store-init "default")))
      (remhash 'test table)
      table)
    (tl--store-flush location)))

;;;###autoload
(defun tl-store-member-p (key &optional location)
  "Return t if KEY in LOCATION exists.
LOCATION defaults to `tl-store-location'."
  (let ((nil-value (format "--nilvalue%s--" (current-time))))
    (not (equal (tl-store-get key location nil-value)
                nil-value))))

;;;###autoload
(defun tl-store-clear (&optional location)
  "Clear the store at LOCATION (defaults to `tl-store-location')."
  (let* ((location (or location tl-store-location))
         (path (expand-file-name location tl-store-dir)))
    (remhash location tl--store-table)
    (when (file-exists-p path)
      (delete-file path)
      t)))

(provide 'tl-store)
