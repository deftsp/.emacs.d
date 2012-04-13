;;; tsp-bbdb.el ---
;; Copyright (C) 2008  S.P.Tseng
;; Author: S.P.Tseng <mendouer@163.com>

(setq bbdb-file "~/.bbdb")
(setq bbdb-file-coding-system 'utf-8)
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(setq bbdb-north-american-phone-numbers-p nil
      bbdb-check-zip-codes-p nil
      bbdb-user-mail-names (regexp-opt '("deftsp@gmail.com" "mendouer@163.com" "kirby1985@gmail.com"))
      bbdb-no-duplicates-p nil            ; must be set to nil to edit a list of email addresses
      bbdb-use-pop-up nil
      bbdb-complete-name-allow-cycling t
      bbdb-pop-up-target-lines 5
      bbdb-always-add-addresses 'ask
      bbdb-new-nets-always-primary 'nil
      bbdb-offer-save t
      bbdb-get-only-first-address-p t)

;; (setq bbdb-display-layout-alist
;;       '((one-line
;;          (order groups aka net gnus-score www icq notes)
;;          (name-end . 20)
;;          (toggle . t))
;;         (multi-line
;;          (order aka net mail-alias groups icq www notes creation-date timestamp)
;;          (indention . 14)
;;          (toggle . t))
;;         (pop-up-multi-line
;;          (indention . 14))) )


;; (setq-default bbdb/news-auto-create-p nil)
(setq bbdb/mail-auto-create-p t)

;; (require 'bbdb-query nil t)               ; nice functions: bbdb-append, bbdb-keep, bbdb-flush
;; (add-hook 'mail-setup-hook 'mail-abbrevs-setup)
;; (add-hook 'mail-setup-hook 'bbdb-define-all-aliases)
;; (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
;; (when (require 'message nil t)
;;   (bbdb-insinuate-message))
;; (setq bbdb-hashtable-size 200003)
;; (setq bbdb-pop-up-elided-display-fields '(net))
;; (setq bbdb-file-coding-system 'emacs-mule)
;; (add-to-list 'auto-coding-alist '("\\.bbdb\\'" . utf-8)) ??
;; (setq bbdb-send-mail-style 'message)
;; (setq bbdb-send-mail-style 'compose-mail)
;; (setq bbdb-dwim-net-address-allow-redundancy t)
;; (setq bbdb-completion-type 'primary-or-name)
;; (setq bbdb-define-all-aliases-mode 'all)
(setq bbdb-default-country "China")
;; (setq bbdb-default-phones-label "Telefon")
;; (setq bbdb-print-file-name (if win32p "c:/temp/bbdb.tex" "~/work/emacs/bbdb/bbdb.tex"))
;; (setq bbdb-print-elide '(tex-name aka mail-alias timestamp creation-date))
;; (setq bbdb-print-prolog (concat bbdb-print-prolog "\\input texnansi\n\n"))
;; (setq bbdb-print-require t)
;; (define-key bbdb-mode-map "M" 'bbdb-mail-with-default-mailer)
;; (define-key bbdb-mode-map [(meta ?c)] 'bbdb-category)


;;; Show an address list

;; if you want to start your search from scratch and you don’t want to call M-x bbdb with . as the regular expression,
;; use the universal prefix argument (C-u M-x wicked/bbdb-find-people-with-addresses) and it will search your entire
;; contact database.

(defun wicked/bbdb-find-people-with-addresses (&optional regexp records)
  "Filter the displayed BBDB records to those with addresses."
  (interactive "MRegexp: ")
  (let ((records (if current-prefix-arg (bbdb-records)
                     (or records bbdb-records (bbdb-records))))
        filtered
        cons next)
    (while records
      (when (and (bbdb-record-get-field-internal (if (arrayp (car records))
                                                   (car records)
                                                   (caar records)) 'address)
               (or (null regexp)
                  (string= regexp "")
                  (delq nil
                        (mapcar
                         (lambda (address)
                           (string-match regexp (wicked/bbdb-address-string address)))
                         (bbdb-record-get-field-internal
                          (if (arrayp (car records))
                              (car records)
                              (caar records)) 'address)))))
        (setq filtered (cons (if (arrayp (car records))
                                 (car records)
                                 (caar records)) filtered)))
      (setq records (cdr records)))
    (bbdb-display-records (nreverse filtered))))

(defun wicked/bbdb-address-string (address)
  "Return ADDRESS as a string."
  (mapconcat
   'identity
   (delq nil
         (list
          (mapconcat 'identity (bbdb-address-streets address) ", ")
          (let ((s (bbdb-address-city address))) (and (not (string= s "")) s))
          (let ((s (bbdb-address-state address))) (and (not (string= s "")) s))
          (let ((s (bbdb-address-zip address))) (and (not (string= s "")) s))
          (let ((s (bbdb-address-country address))) (and (not (string= s "")) s))))
   ", "))

(defun wicked/bbdb-yank-addresses ()
  "Copy displayed addresses to the kill ring."
  (interactive)
  (kill-new
   (mapconcat
    (lambda (record)
      (concat
       (bbdb-record-name (car record)) "\n"
       (mapconcat
        (lambda (address)
          (concat (bbdb-address-location address) ": " (wicked/bbdb-address-string address)))
        (bbdb-record-get-field-internal (car record) 'address)
        "\n")))
    bbdb-records
    "\n\n")))


;;; Show a phone list

;; You can use this function to filter phone numbers in your BBDB based on a regular expression. As usual, leaving the
;; regular expression blank means that all records with phone numbers will be displayed. By default, the function works
;; on the currently displayed records, allowing you to apply multiple filters. You can call it with a universal prefix
;; argument (C-u M-x sacha/bbdb-find-people-with-phones) to match against all contacts in your database.

(defun sacha/bbdb-find-people-with-phones (&optional regexp records)
  "Search for phone numbers that match REGEXP in BBDB RECORDS.
Without a prefix argument, filter the list of displayed records.
Call with a prefix argument to search the entire database.  This
works best if you use a consistent format to store your phone
numbers.  The search will strip out non-numeric characters. For
example, +1-888-123-4567 will be treated as +18001234567.

To search for all numbers in Toronto, search for
\"+1\\(416\\|647\\)\". If you search for certain areas
frequently, it might be a good idea to define a function for
them."
  (interactive (list (read-string "Regexp: ")
                     (if current-prefix-arg
                         (bbdb-records)
                         (or bbdb-records (bbdb-records)))))
  (let (filtered next)
    (while records
      (when
          (and (bbdb-record-get-field-internal
                (if (arrayp (car records))
                    (car records)
                    (caar records)) 'phone)
               (or
                (null regexp)
                (string= regexp "")
                (delq nil
                      (mapcar
                       (lambda (phone)
                         (when (string-match regexp (sacha/bbdb-phone-string phone))
                           (concat (bbdb-phone-location phone) ": " (bbdb-phone-string phone))))
                       (bbdb-record-get-field-internal
                        (if (arrayp (car records))
                            (car records)
                            (caar records)) 'phone)))))
        (setq filtered (cons (if (arrayp (car records))
                                 (car records)
                                 (caar records)) filtered)))
      (setq records (cdr records)))
    (bbdb-display-records (nreverse filtered))))

(defun sacha/bbdb-phone-string (&optional phone)
  "Strip non-numeric characters from PHONE, except for +."
  (replace-regexp-in-string "[^+1234567890]" "" (bbdb-phone-string phone)))

(defun sacha/bbdb-yank-phones ()
  "Copy a phone list into the kill ring."
  (interactive)
  (kill-new
   (mapconcat
    (lambda (record)
      (mapconcat
       (lambda (phone)
         (concat (bbdb-record-name (car record)) "\t"
                 (bbdb-phone-location phone) "\t"
                 (bbdb-phone-string phone)))
       (bbdb-record-get-field-internal (car record) 'phone)
       "\n"))
    bbdb-records
    "\n")))

;;; Filtering by Mail Alias

;; You can use "a" (bbdb-add-or-remove-mail-alias) in BBDB buffers to add a mail alias to the current entry, or "* a" to
;; add a mail alias to all displayed entries. I use mail aliases to tag or categorize my contacts (example: emacs,
;; writing, etc.). The following functions can make it easy for you to filter displayed records using a combination of
;; keywords:

;; Display records matching ALIAS and ALIAS     M-x sacha/bbdb-filter-displayed-records-by-alias RET alias alias
;; Display records matching ALIAS or ALIAS  C-u M-x sacha/bbdb-filter-displayed-records-by-alias RET alias alias
;; Omit records matching ALIAS and ALIAS    M-x sacha/bbdb-omit-displayed-records-by-alias RET alias alias
;; Omit records matching ALIAS or ALIAS     C-u M-x sacha/bbdb-omit-displayed-records-by-alias RET alias alias

(defun sacha/bbdb-filter-by-alias-match-all (query-aliases record-aliases)
  "Return non-nil if all QUERY-ALIASES are in RECORD-ALIASES."
  (let ((result t))
    (while query-aliases
      (unless (member (car query-aliases) record-aliases)
        (setq query-aliases nil
              result nil))
      (setq query-aliases (cdr query-aliases)))
    result))

(defun sacha/bbdb-filter-by-alias-match-any (query-aliases record-aliases)
  "Return non-nil if any in QUERY-ALIASES can be found in RECORD-ALIASES."
  (let (result)
    (while query-aliases
      (when (member (car query-aliases) record-aliases)
        (setq query-aliases nil
              result t))
      (setq query-aliases (cdr query-aliases)))
    result))

;; Moved this to a convenience function so that we don't
;; have to deal with invert and property splitting.
(defun sacha/bbdb-filter-by-alias (bbdb-records
                               alias-filter-function
                               query
                               &optional invert)
  "Return only the BBDB-RECORDS that match ALIAS-FILTER-FUNCTION.
ALIAS-FILTER-FUNCTION should accept two arguments:
 - QUERY, a list of keywords to search for
 - aliases, a list of keywords from the record
If INVERT is non-nil, return only the records that do
not match."
  (delq nil
        (mapcar
         (lambda (rec)
           (if (funcall alias-filter-function
                        query
                        (split-string
                         (or (bbdb-record-getprop
                             (if (vectorp rec)
                                 rec
                                 (car rec))
                             'mail-alias) "")
                         "[ \n\t,]+"))
               (when (null invert) rec)
               (when invert rec)))
         bbdb-records)))

;; Splitting this into two functions because of interactive calling.
(defun sacha/bbdb-filter-displayed-records-by-alias (query &optional any)
  "Display only records whose mail-aliases match QUERY.
If ANY is non-nil, match if any of the keywords in QUERY are
present.
See also `sacha/bbdb-omit-displayed-records-by-alias'."
  (interactive (list
                (let ((crm-separator " "))
                  (completing-read-multiple
                   "Mail aliases: "
                   (bbdb-get-mail-aliases)))
                current-prefix-arg))
  (when (stringp query)
    (setq query (split-string query "[ \n\t,]+")))
  (bbdb-display-records
   (sacha/bbdb-filter-by-alias
    (or bbdb-records (bbdb-records))
    (if any
        'sacha/bbdb-filter-by-alias-match-any
        'sacha/bbdb-filter-by-alias-match-all)
    query)))

;; Splitting this into two functions because of interactive calling.
(defun sacha/bbdb-omit-displayed-records-by-alias (query &optional any)
  "Display only records whose mail-aliases do not match QUERY.
If ANY is non-nil, match if any of the keywords in QUERY are
present.

See also `sacha/bbdb-filter-displayed-records-by-alias'."
  (interactive (list
                (let ((crm-separator " "))
                  (completing-read-multiple
                   "Mail aliases: "
                   (bbdb-get-mail-aliases))
                  current-prefix-arg)))
  (when (stringp query)
    (setq query (split-string query "[ \n\t,]+")))
  (bbdb-display-records
   (sacha/bbdb-filter-by-alias
    (or bbdb-records (bbdb-records))
    (if any
        'sacha/bbdb-filter-by-alias-match-any
        'sacha/bbdb-filter-by-alias-match-all)
    query
    t)))



;; add fields to the record creation process in M-x bbdb-create
(defadvice bbdb-read-new-record (after wicked activate)
  "Prompt for the birthdate as well."
  (bbdb-record-putprop ad-return-value 'birthdate
                       (bbdb-read-string "Birthdate (YYYY.MM.DD): ")))

(provide '50bbdb)