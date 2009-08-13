;; $Id: voctest.el 1643 2005-05-26 03:13:36Z abel $

;; voctest.el - an Emacs vocabulary test program

;; Author: Alexander L. Belikoff (alexander @ belikoff . net)
;; Version:  2.0 ($Revision: 1643 $)
;; Keywords: dictionary, vocabulary, test
;; URL: http://www.belikoff.net/software/voctest.el

;; Copyright (C) 1997-2005 Alexander L. Belikoff

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; Voctest allows you to test your vocabulary, based on a dictionary.
;; It displays a word and a number of translations, where only one of
;; them is correct. User responds by pressing a number, corresponding
;; to the "right" translation. If a user presses 'q' (case doesn't matter)
;; the program quits.

;; The following goodies are supported:
;;   'c' - toggle automatic display of the correct answer if the user
;;         makes a mistake (default: OFF)

;;   '?' - give a quick hint for a number of seconds, defined by the value
;;         of the `voctest-hint-time' variable. After a hint, the answer
;;         is not counted.

;; The program is not restricted to translations only. The structure of the
;; dictionary file allows you to use other things - for example to test
;; your knowledge of words' gender in French or German, or the way
;; to write a word in Kanji, based on it's Hiragana spelling (well, I'm
;; not sure the latter will work, since the Japanese codeset may use ':')
;; Moreover, in the future multi-field dictionaries will be supported,
;; thus allowing various types of testing for the same dictionary.

;; The number of choices displayed is defined by the `voctest-num-choices'
;; variable. It must be in the [2..9] region.

;; The function to start is 'voctest'. You will be prompted for a
;; dictionary file to use. By default, the file is looked in the directory,
;; specified by a `voctest-dictionary-directory' variable. The default file
;; is specified by the `voctest-default-file' variable.

;; The dictionary in the file consists of entries. Each entry occupies
;; a line and has the following structure:
;;
;;          field1 : field2 : ... : fieldN
;;
;; for example `field1' may be a word, `field2' - it's article, `field3/ -
;; it's translation in Russian, and `field4' - translation in Hebrew.
;; Spaces and tabs don't matter.

;; The test direction is specified by the `voctest-test-direction' variable.
;; It is a pair of numbers, where the first one is the field number to use
;; (starting with 0) for words in questions, and the second one - the field
;; number to fetch the answer choices. The NIL symbol instead of number will
;; make voctest fetch the last entry in the article

;; For highlighting, the following faces are defined:
;;   voctest-question-face     - for the word in question
;;   voctest-answer-face       - for the answer choices
;;   voctest-right-count-face  - for the `Right' count
;;   voctest-wrong-count-face  - for the `Wrong' count


(defvar voctest-version "2.0 ($Revision: 1643 $)"
  "VOCTEST version")

(defvar voctest-dictionary-directory "~/lib/dic/"
  "Directory where to look for the dictionaries")

(defvar voctest-default-file "DEFAULT.dic"
  "Default dictionary file")

(defvar voctest-num-choices 5
  "Number of answer choices - must be in [2..9]")

(defvar voctest-test-direction '(0 . nil)
      "Test direction - a pair of two numbers.
The first number is an element index in the dictionary article to use for
the word to test. The second number is used to fetch the answers")

(defvar voctest-hint-time 2
  "Number of seconds to display hint for")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; requirements


;; mode-specific tables

(defvar voctest-mode-syntax-table nil
  "Syntax table used while in voctest mode.")

(cond ((not voctest-mode-syntax-table)
       (setq voctest-mode-syntax-table (make-syntax-table))
       (modify-syntax-entry ?\" ".   " voctest-mode-syntax-table)
       (modify-syntax-entry ?\\ ".   " voctest-mode-syntax-table)
       (modify-syntax-entry ?' "w   " voctest-mode-syntax-table)))

(defvar voctest-mode-abbrev-table nil
  "Abbrev table used while in voctest mode.")

(define-abbrev-table 'voctest-mode-abbrev-table ())

(defvar voctest-mode-map nil)

(cond ((not voctest-mode-map)
       (setq voctest-mode-map (make-sparse-keymap))
       (define-key voctest-mode-map "1" #'voctest-user-selection)
       (define-key voctest-mode-map "2" #'voctest-user-selection)
       (define-key voctest-mode-map "3" #'voctest-user-selection)
       (define-key voctest-mode-map "4" #'voctest-user-selection)
       (define-key voctest-mode-map "5" #'voctest-user-selection)
       (define-key voctest-mode-map "6" #'voctest-user-selection)
       (define-key voctest-mode-map "7" #'voctest-user-selection)
       (define-key voctest-mode-map "8" #'voctest-user-selection)
       (define-key voctest-mode-map "9" #'voctest-user-selection)
       (define-key voctest-mode-map "c" #'voctest-toggle-auto-correction)
       (define-key voctest-mode-map "?" #'voctest-hint)
       (define-key voctest-mode-map "q" #'voctest-quit)
       (define-key voctest-mode-map "Q" #'voctest-quit)))


;; faces and font-lock

(defmacro voctest-make-face (face &optional proto fg bg fn)
  "This macro checks whether the face FACE exists, and if it doesn't, it
copies it from the face PROTO (if non-nil) or creates it. The FG
argument is a sting containing a color name.  If it is supplied, it is
used to define the new face's foregroung color.  The same applies to
BG and FN arguments - they are used to set the face's backgroung and
font respectively.

Examples:

  (voctest-make-face 'foo-some-face nil)
  (voctest-make-face 'foo-other-face 'bold \"DarkGreen\")
  (voctest-make-face 'foo-one-more-face nil \"DarkGreen\" \"Yellow\" \"6x13\")


\(voctest-make-face FACE &optional PROTO FG BG FN\)"

  `(if (not (facep ,face))
       (progn
	 (if ,proto
	     (copy-face ,proto ,face)
	   (make-face ,face))
	 (if ,fg
	     (set-face-foreground ,face ,fg))
	 (if ,bg
	     (set-face-background ,face ,bg))
	 (if ,fn
	     (set-face-font ,face ,fn)))))


(voctest-make-face 'voctest-right-count-face 'bold "DarkGreen")
(voctest-make-face 'voctest-wrong-count-face 'bold "Red")
(voctest-make-face 'voctest-question-face nil "White" "Blue")
(voctest-make-face 'voctest-answer-face)


(defconst voctest-font-lock-keywords
 '(
   ("^[ \t]*\\(Right:[ \t]+[0-9]+\\)[ \t]+/[ \t]+\\(Wrong:[ \t]+[0-9]+\\)[ \t]*$"
    (1 'voctest-right-count-face)
    (2 'voctest-wrong-count-face))
   ("^[ \t]*===>[ \t]+\\(.*\\)[ \t]+<===[ \t]*$"
    (1 'voctest-question-face))
   ("^[ \t]*[1-9]:[ \t]+\\(.*\\)[ \t]*$"
    (1 'voctest-answer-face))
   ))


;; mode activation routines

(defun voctest-mode ()
  "Major mode for Vocabulary test
Special commands:

\\{voctest-mode-map}

Turning on text-mode runs the hook `voctest-mode-hook'."
  
  (interactive)
  (kill-all-local-variables)

  (use-local-map voctest-mode-map)
  (setq mode-name "voctest")
  (setq major-mode 'voctest-mode)
  (setq local-abbrev-table voctest-mode-abbrev-table)
  (set-syntax-table voctest-mode-syntax-table)
  (run-hooks 'voctest-mode-hook))


;; activation

(defun voctest (&optional server port nick full-name)
  "Run VOCTEST"

  (interactive "")
  (if (or (< voctest-num-choices 2)
	  (> voctest-num-choices 9))
      (error (format "Bad number of choices (%d)" voctest-num-choices)))
  
  (let* ((file (read-file-name
		"Dictionary: " voctest-dictionary-directory
		voctest-default-file t nil))
	 (bufname (concat "*Vocabulary Test: " file "*"))
	 (buffer (get-buffer-create bufname)))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (voctest-mode)
    (setq buffer-read-only t)

    ;; make local variables

    (make-variable-buffer-local 'dic)
    (make-variable-buffer-local 'current-fetch)
    (make-variable-buffer-local 'current-question)
    (make-variable-buffer-local 'current-answer)
    (make-variable-buffer-local 'auto-correct)
    (make-variable-buffer-local 'hinted)
    (make-variable-buffer-local 'rights)
    (make-variable-buffer-local 'wrongs)
    (make-variable-buffer-local 'mistakes)
    (make-local-variable 'font-lock-defaults)

    (setq rights 0
	  wrongs 0
	  auto-correct nil
	  hinted nil
	  mistakes ())
    (setq dic (voctest-load-dictionary file))
    (random t)
    (setq font-lock-defaults '(voctest-font-lock-keywords))
    (turn-on-font-lock)
    (voctest-go)))


;; User I/O

(defun voctest-display (fetch dic)
  "Display the choices base on the random FETCH and dictionary DIC"

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format
	     "                                                   Right: %-4d / Wrong: %-4d\n"
	     rights wrongs))
    (insert (format "\n       ===> %s <===\n\n\n" current-question))

    (let ((i 1))
      (mapcar (lambda (n)
		(insert (format "    %d:  %s\n" i (voctest-meaning n dic)))
		(setq i (1+ i)))
	      (nth 1 fetch)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))


(defun voctest-user-selection ()
  "Handler of user's choice"

  (interactive "")
  (let ((xx (assoc (if (string-match "[Xx][Ee]macs" emacs-version)
		       last-input-char
		     last-input-event)
		   '((?1 . 1) (?2 . 2)  (?3 . 3) (?4 . 4) (?5 . 5)
		     (?6 . 6) (?7 . 7) (?8 . 8) (?9 . 9)))))
;;    (message (format "char: %S  ev: %S" last-input-char last-input-event))
    (cond
     ((and xx 
	   (<= (cdr xx) (length (nth 1 current-fetch))))
      (voctest-process-answer (cdr xx) current-fetch)
      (voctest-go))
     (t
      (ding)))))
      

(defun voctest-toggle-auto-correction ()
  "Toggle automatic wrong answer correction"

  (interactive "")
  (setq auto-correct (not auto-correct))
  (message (concat "Automatic correction is "
		   (if auto-correct
		       "ON"
		     "OFF"))))
      

(defun voctest-hint ()
  "Display a quick hint"

  (interactive "")
  (message (format "%s:  %s" current-question current-answer))
  (sit-for voctest-hint-time)
  (message "")
  (setq hinted t))
      

(defun voctest-quit ()
  "Quit voctest"

  (interactive "")
  (let ((buf (current-buffer)))
    (if (and (> wrongs 0) (y-or-n-p "Display list of mistakes? "))
	(voctest-show-mistakes))
    (kill-buffer buf)))


(defun voctest-go ()
  "Fetch the articles randomly and display a screen"

  (setq hinted nil)
  (setq current-fetch
	(voctest-uniqify-fetch
	 (voctest-random-select voctest-num-choices (length dic))
	 dic))
  (let ((idx (voctest-correct-index current-fetch)))
    (setq current-question (voctest-word idx dic))
    (setq current-answer (voctest-meaning idx dic)))
  (voctest-display current-fetch dic))
  

(defun voctest-process-answer (i fetch)
  "Check the correctness of an answer I based on FETCH.
Process wrong answers"

  (cond
   ((= (1- i) (car fetch))
    (if (not hinted)
	(setq rights (1+ rights)))
    (message "Right"))

   (t
    (if (not hinted)
	(progn
	  (setq wrongs (1+ wrongs))
	  (voctest-add-to-mistakes current-question current-answer)))
    (if auto-correct
	(message (format "Wrong!  %s: %s" current-question current-answer))
      (message "Wrong!"))
    (ding))))


(defun voctest-random-select (num max)
  "Return a pair (N . L), where L is a list of NUM random numbers
(each one in [0,MAX) range), and N is a random number in [0..NUM) range"

  (if (or (>= num max)
	  (<= max 1))
      (error (format "Cannot select %d different numbers in 0..%d"
		     num
		     (- max 1)))
    (let ((nl nil))
      (while (< (length nl) num)
	(let ((n (random max)))
	  (if (not (member n nl))
	      (setq nl (cons n nl)))))
      (cons (random num) (list nl)))))


(defun voctest-uniqify-fetch (fetch dic)
  "Make sure the choices (translations) in the fetch are unique"

  (let ((nset ())
	(ltr nil)
	(nx nil)
	(oidx (car fetch))
	(nidx -1)
	(oset (nth 1 fetch))
	(i 0))
    (while (< i (length oset))
      (let* ((n (nth i oset))
	     (m (voctest-meaning n dic))
	     (found (member m ltr)))
	(cond
	 (found
	  (if (= i oidx)
	      (setq nidx (- (length ltr) (length found)))))
	 (t
	  (if (= i oidx)
	      (setq nidx (length nset)))
	  (setq nset (append nset (list n)))
	  (setq ltr (append ltr (list m)))))
;	(insert (format "\n\ni: %d  n: %d  m: %s\nltr: %S\nnidx: %d  oidx: %d\nfound: %S\n\n" i n m ltr nidx oidx found))
	)
      (setq i (1+ i)))
    (cons nidx (list nset))))


;; service functions

(defun voctest-word (n dic)
  "Return an entry from article number N in the dictionary DIC. The entry
number is defined by the first element of the `voctest-test-direction'
direction"

  (let ((i (car voctest-test-direction))
	(a (nth n dic)))
    (if i
	(nth i a)
      (nth (1- (length a)) a))))


(defun voctest-meaning (n dic)
  "Return an entry from article number N in the dictionary DIC. The entry
number is defined by the second element of the `voctest-test-direction'
direction"

  (let ((i (cdr voctest-test-direction))
	(a (nth n dic)))
    (if i
	(nth i a)
      (nth (1- (length a)) a))))


(defmacro voctest-correct-index (fetch)
  "Return the index of the correct answer in the dictionary, based on
the fetch FETCH"

  `(nth (car ,fetch) (nth 1 ,fetch)))


;; mistakes list

(defun voctest-clear-mistakes ()
  "Clear the mistakes list"
  
  (setq mistakes ()))


(defun voctest-add-to-mistakes (word meaning)
  "Add WORD and it's MEANING to the list of mistakes. Note, that each
WORD is added only once"

  (if (not (assoc word mistakes))
      (setq mistakes (cons (cons word meaning) mistakes))))


(defun voctest-show-mistakes ()
  "Display the list of mistakes"

  (let ((l mistakes)
	(mbuf (get-buffer-create "*Voctest: mistakes*")))
    (set-buffer mbuf)
    (erase-buffer)
    (mapcar (lambda (e)
	      (insert (format "%-30s :    %s\n" (car e) (cdr e))))
	    l)
    (switch-to-buffer mbuf)))


;; dictionary I/O

(defun voctest-load-dictionary (file)
  "Load a dictionary from FILE and return it"

  (let ((b (find-file-read-only file))
	(dl nil)
	(s ""))
    (while s
      (let ((e (voctest-parse-line s)))
	(if e
	    (setq dl (cons e dl))))
      (setq s (voctest-read-next-line-from-buffer b)))
    (kill-buffer b)
    dl))


(defun voctest-read-next-line-from-buffer (b)
  "Read the current line from buffer B, move the point down
and return the line"
  
  ;; There may be a possible problem with a non-saved excursion
  ;; but if we do save it, we won't be able to remember the new
  ;; position in this buffer
  
  (let ((cb (current-buffer))
	(s nil))
    (set-buffer b)
    (cond
     ((not (equal (point) (point-max)))
      (setq s (buffer-substring-no-properties
	       (progn (beginning-of-line) (point))
	       (progn (end-of-line) (point))))
      (beginning-of-line 2)))
    (set-buffer cb)
    s))


(defun voctest-parse-line (s)
  "Parse line S from a dictionary and split it to the list of entries"

  (let ((doit t)
	(st 0)
	(res ()))
    (while doit
      (cond
       ;; comment
       ((string-match "[ \t]*#" s st)
	(setq doit nil))
       
       ((string-match "[ \t]*\\([^:]*\\):" s st)
	(setq st (match-end 0))
	(let ((e (voctest-cleanup-entry (match-string 1 s))))
	  (if e
	      (setq res (cons e res))))
	)

       (t
	;; last item
	(if (string-match "[ \t]*\\(.*\\)$" s st)
	    (let ((e (voctest-cleanup-entry (match-string 1 s))))
	      (if e
		  (setq res (cons e res)))))
	(setq doit nil))))
    (reverse res)))


(defun voctest-cleanup-entry (s)
  "Strip leading and trailing spaces in the entry S"

  (if (string-match "^[ \t]*\\([^ \t].*[^ \t]\\)[ \t]*$" s)
      (match-string 1 s)
    nil))


(provide 'voctest)
