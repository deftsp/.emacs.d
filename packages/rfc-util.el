;;; rfc-util.el --- RFC-util interface for emacs.

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: rfc
;; Version: 1.0

;; This file is [not yet] part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; An interface to rfc-util to download RFC's and view them.  Keeps RFCs on disk
;; so that you can view them in the future.  You will need to install rfc-util
;; in order to use this.

;; You can download RFC-util from http://www.dewn.com/rfc/

;; (defvar rfc-directory "~/.rfc-util" "Destination to store downloaded RFCs")
(defvar rfc-directory "~/.rfcs" "Destination to store downloaded RFCs")

(defun rfc()
  "Prompt for an rfc number and display it in a new buffer"
  (interactive)

  (if (not (file-exists-p rfc-directory))
      (make-directory rfc-directory))

  (setq rfc-number (read-string "What RFC number: "))

  ;;no reason to download if the file already exists
  (if (file-readable-p (concat rfc-directory "/rfc" rfc-number ".txt"))
      (rfc-open rfc-number)
      (rfc-fetch rfc-number)))


(defun rfc-open(rfc-number)
  (find-file (concat rfc-directory "/rfc" rfc-number ".txt")))

(defun rfc-fetch(rfc-number)

  (message "Fetching RFC... please wait.")

  (shell-command (concat "rfc -d " rfc-number " " rfc-directory "/"))

  (message "DONE fetching RFC")


  (beginning-of-buffer))


(provide 'rfc-util)
