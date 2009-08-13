;;; man-completion.el --- completion for M-x man

;; Copyright 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 5
;; Keywords: data
;; URL: http://www.geocities.com/user42_kevin/man-completion/index.html
;; EmacsWiki: ManMode

;; man-completion.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; man-completion.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code extends M-x man with completion of man page names and
;; filenames, and a new default page name at point which collapses hyphens
;; (like nroff uses in formatted pages) and can optionally transform some
;; perl class names.
;;
;; iman.el does similar page name completion, with info documents too, but
;; not filename completion.  As of rev 2.22 it has the section number like
;; "cat(1)" and sets "require-match" so if you've got cat(1) and catdoc(1)
;; then you end up having to type "cat(" to differentiate, which is a bit
;; annoying.
;;
;; woman.el does a similar completion too, by going through /usr/share/man
;; etc to get a filename.  That's also what "bash_completion" does
;; (http://www.caliban.org/bash/index.shtml#completion).  But the strategy
;; in man-completion.el is to ask man what pages are available, rather than
;; digging.  It can potentially get aliases man has found with lexgrog but
;; which don't have symlinks (or ".so" pages).

;;; Install:

;; Put man-completion.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "man" '(require 'man-completion))
;;
;; To use the smart perl module name at point default, add also
;;
;;     (setq man-completion-at-point-functions
;;           '(man-completion-transform-perl
;;             man-completion-transform-poco))
;;
;; There's an autoload cookie for this below, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and 22, works in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - avoid truncation to tty width on very long page names
;; Version 3 - new page at point transform scheme
;; Version 4 - disallow "[" except for sole "[" alias of "test(1)"
;; Version 5 - cope with non-existent `default-directory'

;;; Code:

;;;###autoload (eval-after-load "man" '(require 'man-completion))

(require 'man)

;; xemacs21 incompatibilities
;;
(defsubst man-completion-replace-regexp-in-string
  (regexp rep string fixedcase literal)
  "`replace-regexp-in-string' made available in xemacs."
  (if (eval-when-compile (fboundp 'replace-regexp-in-string))
      (replace-regexp-in-string regexp rep string fixedcase literal)
    ;; note FIXEDCASE always true
    (replace-in-string string regexp rep literal)))

;; emacs21 fallbacks
(defun man-completion-file-name-completion (part dir pred)
  "`file-name-completion' adapted for 2 args in emacs21.
The PRED is ignored, which is not really right, but should be ok
for now."
  (condition-case nil
      (file-name-completion part dir pred)
    (error (file-name-completion part dir))))


;;----------------------------------------------------------------------------
;; modifying the guessed page at point

(defcustom man-completion-at-point-functions nil
  "List of functions to modify `man-completion-at-point'.
Each function is called (FUNC STR) with the apparent page name.
FUNC can transform STR and return a new string which goes on to
the further functions, or it can return either nil or STR
unchanged if it's got nothing to apply.

Caution: This is slightly experimental.  Maybe page name
transformations could be better applied to the user-entered
string too, or instead."

  :group   'man
  :type    'hook
  :options '(man-completion-transform-perl
             man-completion-transform-poco))

(defun man-completion-transform-perl (str)
  "Expand or contract a perl package name.
This function is designed for `man-completion-at-point-functions'.

* A perl package name suffix like \"Xyzzy\" expands to say
  \"Some::Thing::Xyzzy\" if that's the only Xyzzy available.  A
  multipart \"Xx::Yy\" can expand to \"Some::Thing::Xx::Yy\" too.
  This is good when a long base part is omitted from
  documentation (or even from code in suitable contexts), eg. in
  POE and Perl::Critic.

* A qualified perl name like \"Foo::Bar::Quux::something\" is
  tried first in full then reduced to \"Foo::Bar::Quux\",
  \"Foo::Bar\" or even just \"Foo\".  This is good for getting
  the package name out of a function name or sub-package.
  Sometimes it's too aggressive, but it's only for the default,
  so you can see what it did before accepting.

These actions are only applied if STR has at least one upper case
letter, since that's usual for perl and it means plain shell
command pages are looked up without loading the page name cache."

  (and (let ((case-fold-search nil))
         (string-match "[A-Z]" str))
       ;; don't change if STR is already an exact match
       (not (assoc str (man-completion-cache)))
       ;; prefix expansion first and if it produces a match then don't
       ;; need to prune
       (or (man-completion-perl-expand-prefix str)
           (man-completion-perl-prune-suffix str))))

(defun man-completion-perl-expand-prefix (str)
  (eval-and-compile (require 'cl))
  (let* ((case-fold-search nil)
         (matches (remove* (concat "\\(\\`\\|::\\)"
                                   (regexp-quote str) "\\'")
                           (mapcar 'car (man-completion-cache))
                           :test-not 'string-match)))
    (setq matches (mapcar 'man-completion-name-sans-section matches))
    (setq matches (remove-duplicates matches :test 'string-equal))
    (and (= 1 (length matches))
         (car matches))))

(defun man-completion-perl-prune-suffix (str)
  ;; stop at pruned STR found or set to nil when no more "::"s
  (while (and (not (assoc str (man-completion-cache)))
              (setq str (and (string-match "\\(.*\\)::" str)
                             (match-string 1 str)))))
  str)

(defun man-completion-transform-poco (str)
  "Expand PoCo to POE::Component.
This function is designed for `man-completion-at-point-functions'.

\"PoCo\" is expanded to \"POE::Component\", either alone, or like
\"PoCo::Client::DNS\" to \"POE::Component::Client::DNS\".  PoCo
is used in various POE docs but man pages are under the full
name.

This is only of interest if you're using POE, but it does no harm
to have it turned on all the time."

  (let ((case-fold-search nil))
    (and (string-match "\\`PoCo\\(::\\|\\'\\)" str)
         (concat "POE::Component" (substring str (match-beginning 1))))))

(defun man-completion-name-sans-section (str)
  "Return man page name STR without section number.
A leading section like \"1 foo\" or trailing section like
\"foo(1)\" are both stripped to give \"foo\"."
  (if (string-match "\\`.* \\(.*\\)" str)
      (setq str (match-string 1 str)))
  (if (string-match "\\(.*\\)(.*)\\'" str)
      (setq str (match-string 1 str)))
  str)


;;----------------------------------------------------------------------------
;; page name at point

;; `man-completion-at-point-regexp' expresses what is a man page name.
;;
;; `chars' is the permitted characters.  The aim is to make this tight to
;; avoid punctuation in text or operators in program code.  `chars'
;; deliberately doesn't use the syntax table definition of a symbol char etc
;; so as to be mode-independent.  You can easily find man page names in
;; comments not really related to the programming language at hand.
;;
;; "." and "-" are allowed in the middle of a page name, but not as the
;; first or last char.  Disallowing a final dot in particular helps a page
;; name at the end of a sentence.  "+" is allowed in the middle or at the
;; end, but not at the start.  "+" at the end is needed for say "g++" and
;; other C++ stuff.
;;
;; "[" and "]" are disallowed like other parentheses, but there's a special
;; case in `man-completion-at-point' for "[" alone, the alias of the shell
;; "test" command.
;;
;; `man-completion-hyphen-regexp' matches a hyphenated line break like nroff
;; spits out, with dash chars in ascii, latin1 and unicode, all of which may
;; be used by groff according to its output mode.  Nroff breaks man page
;; names rather a lot in its output, unfortunately, so collapsing these is
;; important.  A hyphen is only recognised after two or more chars, since
;; pretty sure nroff doesn't break after just one.
;;
;; "::" is allowed for perl module names like B::Lint etc.  Each part must
;; be words per the `chars' rule above.  Don't think ":" occurs in a page
;; name apart from doubled for perl, but maybe that could be loosened (which
;; would simplify the regexp too).  A ":" at the end is disallowed though,
;; since that'd most often be punctuation.
;;
;; A "::" is deliberately not matched at the end, for the benefit of perl
;; code writing say Glib::Object:: for a class name.  In that case just
;; Glib::Object is the page name.
;;
;; A digit is allowed at the start of a page name.  This is unusual, but
;; found in things like the Debian "822-date" program.
;;
;; A section suffix like "chmod(2)" is matched only as a digit "(2)" or
;; digit and alnums like "(3X)" or "(3ncurses)".  The match is kept tight so
;; that if you've got program code like "exit(errors ? 0 : 1)" it won't get
;; the arg in parens, just the "exit".  Clearly this can be tricked by say
;; "exit(1)" as code, but a space is not matched so GNU style "exit (1)" is
;; ok.  It's expected that in comments or a document you won't put a space
;; before a section number.
;;
;; Alpha-only section IDs were, maybe still are, found on some non-free
;; systems.  One system of evil memory, which best remain nameless, but
;; which had letters S, C, and O in its name, had say "(S)" for system
;; commands or something like that.  Really not too interested in doing
;; anything for those at the moment.
;;
(defconst man-completion-hypen-regexp
  (concat "["
          ;; xemacs21 doesn't coding-system-p or decode-char, so use
          ;; coding-system-list and decode-coding-string (of course you need
          ;; mule-ucs to have utf-8 in xemacs21 at all though).
          (and (memq 'utf-8 (coding-system-list))
               (let ((str (string #xE2 #x80 #x90))) ;; U+2010 HYPHEN
                 (if (eval-when-compile (fboundp 'string-make-unibyte))
                     (setq str (string-make-unibyte str)))
                 (decode-coding-string str 'utf-8)))
          ;; latin1 #xAD SOFT HYPHEN
          (decode-coding-string (string #xAD) 'iso-8859-1)
          "-]\n[ \t]*"))
(defconst man-completion-at-point-regexp
  ;; No [:alnum:] etc in xemacs21, so fallback to A-Z etc.  Non-ascii in
  ;; page names is probably unlikely what with the scope for it to go wrong
  ;; between filename coding, file content coding and emacs read coding.
  ;; But match non-ascii so a word with accented chars is taken whole, not
  ;; just an ascii portion of it at point.  The fallback doesn't get that
  ;; for xemacs21, what would be better there?
  ;;
  (let* ((alnum (if (string-match "[[:alnum:]]" "A") "[:alnum:]" "A-Za-z0-9"))
         (digit (if (string-match "[[:digit:]]" "0") "[:digit:]" "0-9"))
         (chars (concat alnum "/_"))
         (word  (concat "[" chars ".]"              ;; first, maybe only, char
                        "\\(\\([" chars ".+-]\\|"   ;; middle chars and hyphens
                        man-completion-hypen-regexp "\\)*"
                        "[" chars "+]"              ;; last char
                        "\\)?")))
    (concat word
            "\\(::" word "\\)*"              ;; perl "::" module name parts
            "\\(([" digit "][" alnum "_]*)\\)?"))) ;; optional section suffix

(defun man-completion-at-point ()
  "Return a man page name string at point, or nil if none.
There's no checking whether the page exists, this only picks out
something from the buffer that might be a page name.

A section number like \"(2)\" in \"chmod(2)\" is included if
present.  A hyphenated line break is recognised and collapsed
out (good for man output where nroff hyphenates words)."

  (eval-and-compile
    (require 'thingatpt))

  (save-excursion
    ;; if point is just after a word then go back one char to let
    ;; thing-at-point-looking-at match on that previous word
    (and (not (bobp))
         (save-excursion
           (goto-char (1- (point)))
           (looking-at "\\S-\\(\\s-\\|\\'\\)"))
         (goto-char (1- (point))))

    (let ((str (or
                ;; special case for "[", ie. the shell "test" command, but
                ;; match it only surrounded by whitespace
                (and (thing-at-point-looking-at
                      "\\(\\`\\|\\s-\\)\\(\\[\\)\\(\\s-\\|\\'\\)")
                     "[")

                ;; normal cases, match and strip hyphens
                (and (thing-at-point-looking-at man-completion-at-point-regexp)
                     (man-completion-replace-regexp-in-string
                      man-completion-hypen-regexp "" (match-string 0) t t)))))
      (when str
        (dolist (func (if (functionp man-completion-at-point-functions)
                          (list man-completion-at-point-functions)
                        man-completion-at-point-functions))
          (setq str (or (funcall func str) str))
          (or (stringp str)
              (error "Not a string from %s" func))))
      str)))

;; Not really making direct use of thing-at-point here, but it doesn't hurt
;; to offer this "thing" through that mechanism too.  Could offer
;; bounds-of-thing-at-point etc too, except that wouldn't get hyphens
;; stripped or the man-completion-at-point-functions transforms.
;;
(put 'man-completion-default 'thing-at-point 'man-completion-at-point)


;;----------------------------------------------------------------------------
;; page name completion

(defvar man-completion-cache 'uninitialized
  "An alist of available man pages for completion.
Each entry is (NAME-STRING . nil), there's no value part
currently.  When not yet initialized this is the symbol
`uninitialized' instead of a list.  Function
`man-completion-cache' initializes if not already done.

For a plain page like \"cat\" there's two entries, the plain name
and with its section

    \"cat\"
    \"1 cat\"

If there two pages with the same name in different sections then
parenthesized section numbers are included too, so as to present
those alternatives in completion.  (The plain name means
completion stops there instead of after the \"(\".)  Eg.

    \"chmod\"
    \"chmod(1)\"
    \"chmod(2)\"
    \"1 chmod\"
    \"2 chmod\"")

;; It's tempting to have something watch file date/times for automatic reset
;; on new page installation.  With man-db the /var/cache/man/index.db plus
;; some of the lang-specific /var/cache/man/pt/index.db etc might work.
;; Depends how often you install new stuff.
;;
(defun man-completion-reset ()
  "Discard data cached for `man-completion-read'.
This can be used to get newly installed man pages recognised."
  (interactive)
  (setq man-completion-cache 'uninitialized))

(defun man-completion-cache ()
  "Generate and return the `man-completion-cache' list."

  ;; This code isn't blindingly fast if you've got a lot of man pages, but
  ;; the time goes in "man -k" and in reading its output into a buffer, so
  ;; not much can be done.
  ;;
  ;; The man-db system also offers of its page database with "accessdb".
  ;; accessdb seems a touch quicker than "man -k", but the latter should be
  ;; adequate for now.  (The cutest thing might be direct dbm access to the
  ;; man-db database, if it can do partial key searches, and if you didn't
  ;; mind getting intimate with stuff normally internal to man-db.)
  ;;
  (when (eq 'uninitialized man-completion-cache)
    (message "Building man page completions ...")
    (with-temp-buffer
      (setq default-directory "/") ;; in case inherited doesn't exist

      ;; "man -k" truncates long page names to the tty width, so set COLUMNS
      ;; to avoid that; and force process-connection-type to a pipe so man
      ;; can't maybe get the width on the pseudo-tty TIOCGWINSZ instead of
      ;; COLUMNS.
      ;;
      (let ((process-environment (copy-sequence process-environment))
            (process-connection-type nil))
        (setenv "COLUMNS" "999")
        (call-process "man" nil t nil "-k" ""))
      (goto-char (point-min))

      ;; Putting "-k " and "-l " in the completions list makes them easily
      ;; type-able with a space, since otherwise minibuffer-complete-word
      ;; objects to a space and you have to C-q to type it.  Surely there's
      ;; a better way...?
      ;;
      ;; "-k keyword" works in both emacs and xemacs.  In emacs the message
      ;; "can't find the '-k xyzzy' page" reads like it thought the whole
      ;; thing was a page name, but that's not the case, it just means there
      ;; was nothing matching the keyword.
      ;;
      (let ((ret '(("-k ") ("-l ")))
            prev prevsect)

        ;; "man -k" gives lines like
        ;;     cat (1)              - concatenate files and print ...
        ;; or for an "alias"
        ;;     boot-scripts (7) [boot] - General description of boot sequence
        ;;
        (while (looking-at "^\\(.*?\\) (\\([^)]+?\\))")
          (let ((name (match-string 1))
                (sect (match-string 2)))

            ;; If previous name was the same then put it with the prevsect
            ;; like "chmod(1)" and also the present sect like "chmod(2)".
            (if (equal name prev)
                (progn
                  (setq ret (cons (list (concat name "(" sect ")"))
                                  ret))
                  (when prevsect
                    (setq ret (cons (list (concat prev "(" prevsect ")"))
                                    ret))
                    (setq prevsect nil)))

              ;; If previous name different then just a solitary entry for
              ;; this one, like "chmod".
              (setq ret (cons (list name) ret))
              (setq prev name)
              (setq prevsect sect))

            ;; always an entry like "1 chmod" or "2 chmod" with the sect
            (setq ret (cons (list (concat sect " " name))
                            ret)))
          (forward-line))

        (setq man-completion-cache (nreverse ret))))
    (message ""))
  man-completion-cache)

(defun man-completion-handler (str pred action)
  "Perform completion for `man-completion-read'."
  (if (string-match "^\\(-l \\)\\(.*\\)\\|[./]" str)
      ;; filename
      (let* ((before (or (match-string 1 str) ""))
             (fname  (or (match-string 2 str) str))
             (dir    (or (file-name-directory fname) default-directory))
             (part   (file-name-nondirectory fname)))
        (cond ((eq action nil)
               (let ((ret (man-completion-file-name-completion part dir pred)))
                 (if (stringp ret)
                     (concat before dir ret)
                   ret)))
              ((eq action t)
               (mapcar (lambda (s) (concat before dir s))
                       (file-name-all-completions part dir)))
              ((eq action 'lambda)
               (file-exists-p fname))))
    ;; pagename
    (let ((list (man-completion-cache)))
      (cond ((eq action nil)
             (try-completion str list pred))
            ((eq action t)
             (all-completions str list pred))
            ((eq action 'lambda)
             (if (eval-when-compile
                   (fboundp 'test-completion)) ;; new in emacs22
                 (test-completion str list pred)
               (eq t (try-completion str list pred))))))))

(defun man-completion-read ()
  "Read the name of a man page.
A page name at point is offered as a default, and completions
come from the pages reported by \"man -k\" (its \"apropos\"
database).

A filename can also be entered too, with completion, as ./foo,
/dir/foo or -l foo.  A leading \".\", \"/\", or \"-l \" is
recognised for filename completion."

  (let* ((default  (man-completion-at-point))
         (pagename (completing-read
                    (if default
                        (format "Manual entry (default %s): " default)
                      "Manual entry: ")
                    'man-completion-handler
                    nil  ;; predicate
                    nil  ;; require-match
                    nil  ;; initial-input
                    (if (eval-when-compile (boundp 'Man-topic-history))
                        'Man-topic-history             ;; emacs
                      'Manual-page-minibuffer-history) ;; xemacs
                    default)))
    (or pagename default "")))

(defadvice man (before man-completion activate)  ;; emacs
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))

(defadvice manual-entry (before man-completion activate)  ;; xemacs
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))

(provide 'man-completion)

;;; man-completion.el ends here
