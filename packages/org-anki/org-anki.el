;;; org-anki.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: anki

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Work with AnkiConnect(https://foosoft.net/projects/anki-connect/)
;;
;; TODO:
;; - Support cloze type card
;; - Use card types to get field instead of hard-coded
;; - Interactive change context style for code like content

;;; Code:
(require 'dash)
(require 's)
(require 'ht)
(require 'request)
(require 'json)

(eval-when-compile
  (require 'cl))

(defvar org-anki--required-anki-connect-version 5
  "Version of the anki connect plugin required")

(defvar org-anki-connect-url "http://127.0.0.1:8765"
  "URL for AnkiConnect")


(defvar org-anki--decks nil
  "List of anki deck names. Update with `'org-anki-update-decks'")

(defvar org-anki--model-names nil)

(defvar org-anki--card-types nil)

(defvar-local org-anki--capture-data nil)
(defvar-local org-anki--context nil)
(defvar-local org-anki--entity-alist nil)

;; NOTE: the origin https://github.com/tkf/emacs-request have problem when the
;; json boy have UTF-8 character use https://github.com/abingham/emacs-request
(defun org-anki-request (callback method params sync)
  (let ((data (json-encode (-non-nil
                            `(("action"  . ,method)
                              ("version" . ,org-anki--required-anki-connect-version)
                              ,(when params
                                 `("params"  . ,params)))))))
    ;; (setq test-post-data data)
    (request org-anki-connect-url
             :type "POST"
             :data data
             :headers '(("Content-Type" . "application/json"))
             :parser 'json-read
             :sync sync
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (unless data
                           (error "org-anki-request got null response"))

                         (let ((error-info (assoc-default 'error data))
                               (result (assoc-default 'result data)))
                           ;; (setq test-result-22 result)
                           (when (vectorp result)
                             (unless (-every? (lambda (x) (not (null x)))
                                              (append result nil))
                               (error "The org-anki-request sub-command got null response")))
                           (if error-info
                               (error error-info)
                             (funcall callback result))))))))

;;;###autoload
(defun org-anki-refresh ()
  (interactive)
  (org-anki-check-version)
  (org-anki-update-model-names)
  (org-anki-update-card-types)
  (org-anki-update-decks))

(defun org-anki-check-version ()
  (org-anki-request
   (lambda (version)
     (when (not (= version org-anki--required-anki-connect-version))
       (error "The AnkiConnect version is %S, but %S is required"
              version org-anki--required-anki-connect-version)))
   "version" nil t))

(defun org-anki-update-model-names ()
  (org-anki-request
   (lambda (result)
     ;; convert vector to list
     (setq org-anki--model-names (append result nil)))
   "modelNames" nil t))


(defun org-anki-update-card-types ()
  (let (card-types)
    (-map (lambda (model-name)
            (org-anki-request
             (lambda (result)
               ;; convert vector to list
               (push (cons model-name (append result nil))
                     card-types))
             "modelFieldNames"
             `(:modelName ,model-name)
             t))
          org-anki--model-names)
    (setq org-anki--card-types card-types)))


(defun org-anki-update-decks ()
  (org-anki-request
   (lambda (decks)
     ;; convert vector to list
     (setq org-anki--decks (append decks nil)))
   "deckNames" nil t))

(defun org-anki-create-basic-card (front back &optional audio-maybe deck model)
  (let* ((deck (or deck "Inbox"))
         (model (or model "org-anki-basic"))
         (audio-url (and audio-maybe (org-anki--get-audio-url front)))
         (audio-file-name (and audio-url (org-anki--get-file-name front)))
         (front-with-audio-maybe (if audio-url (format "%s [sound:%s]" front
                                                       audio-file-name)
                                   front))
         (fields `(:Front ,front-with-audio-maybe :Back ,back))
         (note `(:deckName ,deck :modelName ,model :fields ,fields)))
    (when audio-url
      (let ((l `(:audio (:url ,audio-url :filename ,audio-file-name :fields "Front"))))
        (setq note (append note l))))
    (org-anki-request
     (lambda (ret) (message "Create base card with response: %S" ret))
     "addNotes"
     `(:notes
       [,note])
     t)))


(defun org-anki-create-capture-card (&optional deck model)
  (let ((deck (or deck "Inbox"))
        (model (or model "org-anki-capture"))
        notes)
    (dolist (entity org-anki--entity-alist)
      (let* ((word (assoc-default 'word entity))
             (pronunciation (assoc-default 'pronunciation entity))
             (audio-url (assoc-default 'audio-url entity))
             (audio-name (assoc-default 'audio-name entity))
             (en-definition (assoc-default 'en-definition entity))
             (cn-definition (assoc-default 'cn-definition entity))
             (word-field (if audio-url
                             (format "%s [sound:%s]" word audio-name)
                           word))
             (fields `(:Context ,(org-anki--render-context
                                  org-anki--context word)
                       :Phonetic ,pronunciation
                       :Word ,word-field
                       :EN-Definition ,en-definition
                       :CN-Definition ,cn-definition))
             (note `(:deckName ,deck :modelName ,model :fields ,fields)))
        (when audio-url
          (let ((l `(:audio (:url ,audio-url :filename ,audio-name :fields "Word"))))
            (setq note (append note l))))

        (push note notes)))

    ;; (setq test-note `(:notes ,(vconcat notes)))
    (org-anki-request
     (lambda (ret) (message "Create capture card with response: %S" ret))
     "addNotes"
     `(:notes ,(vconcat notes))
     t)))

(defun org-anki--get-file-name (word)
  (format "yudao-%s-%s.mp3" word (org-id-uuid)))


(defun org-anki-get-word-definition (word callback &optional sync)
  (setq bar word)
  (request (concat "https://api.shanbay.com/bdc/search/?word=" word)
           :type "GET"
           :headers '(("Content-Type" . "application/json"))
           :parser 'json-read
           :sync sync
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (unless data
                         (error "org-anki-request got null response"))

                       (let ((status-code-value (assoc-default 'status_code data))
                             (message-value (assoc-default 'msg data))
                             (data-value (assoc-default 'data data)))
                         (if (= status-code-value 0)
                             (funcall callback data-value)
                           (message message-value)))))))

(defun org-anki-insert-src-block (name level sexp &optional header)
  (let ((title (if header (if (stringp header) header name) nil))
        (indent (make-string level ? )))
    (when title
      (insert (format "%s %s\n" (make-string level ?*) title)))
    (insert (format "%s#+NAME: %s\n" indent name))
    (insert (format "%s#+BEGIN_SRC emacs-lisp :results value\n" indent))
    (insert (format "%s%s\n" indent sexp))
    (insert (concat indent  "#+END_SRC\n"))))

(defun org-anki-capture-protocol (data)
  (setq org-anki--capture-data data)
  (org-anki--create-popup-buffer (plist-get data :body)))

(defun org-anki-capture-region (&optional start end)
  (interactive "r")
  (let ((context (if (use-region-p)
                     (buffer-substring-no-properties start end)
                   (read-string "Context: "))))
    (org-anki--create-popup-buffer context)))

(defun org-anki--create-popup-buffer (context)
  (let ((buf (get-buffer-create "*org-anki*")))
    (with-current-buffer buf
      (org-anki-mode)
      (erase-buffer)
      (setq org-anki--context context)
      (insert "#+STARTUP: content\n\n")
      (org-anki-insert-src-block "Context" 1 'org-anki--context t)
      (insert "* Entities\n")
      (org-anki-insert-src-block "Entities" 1
                                 '(org-anki--pretty-entity-alist))
      (org-anki-refresh-buffer)
      (goto-char (point-max)))
    (pop-to-buffer buf)))

(defun org-anki--span-wrap (s keyword)
  (format "<span class=\"%s\">%s</span>" keyword s))

(defun org-anki--render-definition (definitions)
  (let (collect)
    (if (listp definitions)
        (-map
         (lambda (e)
           (push (format "%s. %s"
                         (car e)
                         (s-join "; " (append (cdr e))))
                 collect))
         definitions)
      (push (s-replace "\n" "; " (s-trim definitions)) collect))

    (s-join "\n" collect)))

(defun org-anki--render-context (context word)
  (s-replace word
             (concat "<span class=\"keyword\">"
                     word
                     "</span>")
             context))

(defun org-anki--pretty-entity-alist ()
  (s-join "\n\n-------\n\n"
          (-map
           (lambda (l)
             (s-join "\n"
                     (-map (lambda (kv)
                             (format "%s: %s" (car kv) (cdr kv))) l)))
           org-anki--entity-alist)))

(defun org-anki-add-word ()
  (interactive)
  (let* ((buf (get-buffer-create "*org-anki*"))
         (words (-uniq (s-split-words (downcase org-anki--context))))
         (word (completing-read "Choose Word: " words)))
    (with-current-buffer buf
      (goto-char (point-max))
      (org-anki-get-word-definition
       word
       (lambda (data-value)
         (setq test-data-value data-value)
         (let ((pronunciation (assoc-default 'pronunciation data-value))
               (us-audio (assoc-default 'us_audio data-value))
               (audio-name (assoc-default 'audio_name data-value))
               (en-definition (assoc-default 'en_definitions data-value))
               (cn-definition (assoc-default 'definition data-value))
               entity)
           (setq entity
                 `((word          . ,word)
                   (pronunciation . ,pronunciation)
                   (audio-url     . ,us-audio)
                   (audio-name    . ,(format "shanbay-%s-%s.mp3"
                                             audio-name
                                             (org-id-uuid)))
                   (en-definition . ,(org-anki--render-definition en-definition))
                   (cn-definition . ,(org-anki--render-definition cn-definition))))

           (push entity org-anki--entity-alist)
           (org-anki-refresh-buffer)
           (goto-char (point-max))))
       t))))

(defun org-anki-quit ()
  (interactive)
  (let ((win (get-buffer-window "*org-anki*")))
    (if (not (one-window-p))
        (delete-window win)
      (bury-buffer))))

(defun org-anki-send-capture-and-quit ()
  (interactive)
  (org-anki-create-capture-card)
  (org-anki-quit))

(defvar org-anki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a r") 'org-anki-refresh)
    (define-key map (kbd "C-c C-a a") 'org-anki-add-word)
    map)
  "Keymap for `org-anki-mode.'")

;;;###autoload
(define-derived-mode org-anki-mode org-mode "Org-Anki"
  "Mode for add Anki entries."
  (run-mode-hooks))

(defun org-anki-refresh-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (while t
          (org-babel-next-src-block)
          (org-babel-execute-maybe)  )
      (user-error
       (if (string= "No further code blocks" (cadr err))
           (message "Refresh Done")
         (message "Unknown user-error: %s" (error-message-string err)))))
    ;; (org-hide-block-all)
    ))

;; (bind-key
;;  "s-t"
;;  (lambda ()
;;    (interactive)
;;    (org-anki-new-capture '(:url "https://jsonformatter.curiousconcept.com/"
;;                            :title "JSON Formatter & Validator"
;;                            :body "Germans have the ability to be brutally frank and Chancellor Merkel is nothing if not frank."))))



(provide 'org-anki)
;;; org-anki.el ends here
