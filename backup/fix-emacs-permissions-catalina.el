;;; package --- Fix permissions for Emacs.app on macOS Catalina
;;; URL: https://gist.github.com/dive/f64c645a9086afce8e5dd2590071dbf9
;;; Author: Artem Loenko
;;; Mail-To: <artyom.loenko@mac.com>
;;; Commentary:
;;; Code:


(defconst _default-emacs-app-plist-path "/Applications/Emacs.app/Contents/Info.plist")
(defconst _temp-buffer-name "*fixing Emacs permissions*")
(defconst _temp-buffer (get-buffer-create _temp-buffer-name))
(with-current-buffer _temp-buffer (erase-buffer))

(defun add-description-if-needed (description_key description)
  "DESCRIPTION_KEY - DESCRIPTION."
  (defconst read-args (list "read" _default-emacs-app-plist-path description_key))
  (if (equal 1 (with-temp-buffer (apply 'call-process "defaults" nil (current-buffer) nil read-args)))
      (progn
	(princ (format "Missing: %s. Adding...\n" description_key) _temp-buffer)
	(defconst write-args (list "write" _default-emacs-app-plist-path description_key "-string" description))
	(apply 'call-process "defaults" nil (current-buffer) nil write-args))
    (princ (format "Existed: %s. Skipping.\n" description_key) _temp-buffer)))

(add-description-if-needed
 "NSDesktopFolderUsageDescription"
 "Emacs requires permission to access the Desktop folder.")
(add-description-if-needed
 "NSDocumentsFolderUsageDescription"
 "Emacs requires permission to access the Documents folder.")
(add-description-if-needed
 "NSDownloadsFolderUsageDescription"
 "Emacs requires permission to access the Downloads folder.")
(add-description-if-needed
 "NSRemovableVolumesUsageDescription"
 "Emacs requires permission to access files on Removable Volumes.")

(switch-to-buffer-other-window _temp-buffer)

;;; fix-emacs-permissions-catalina.el ends here
