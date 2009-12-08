;;; man-completion.el --- completion for M-x man

;; Copyright 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 15
;; Keywords: data
;; URL: http://user42.tuxfamily.org/man-completion/index.html
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
;; iman.el does similar page name completion, and with info documents too,
;; but not filename completion.  As of rev 2.22 it has the section number
;; like "cat(1)" but sets "require-match" so if you've got cat(1) and
;; catdoc(1) then you end up having to type "cat(" to differentiate, which
;; is a bit annoying.
;;
;; woman.el does a similar completion too, by going through /usr/share/man
;; etc to get a filename.  That's also what "bash_completion" does
;; (http://www.caliban.org/bash/index.shtml#completion).  But the strategy
;; here for man-completion.el is to ask man what pages are available, rather
;; than digging.  It can potentially get aliases man knows from its lexgrog
;; parse but which don't have symlinks (or ".so" pages).

;;; Install:

;; Put man-completion.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "man" '(require 'man-completion))
;;
;; To use the smart perl module name at point defaulting, add also
;;
;;     (setq man-completion-at-point-functions
;;           '(man-completion-transform-perl
;;             man-completion-transform-poco))
;;
;; There's an autoload cookie for the `require' below, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - avoid truncation to tty width on very long page names
;; Version 3 - new page at point transform scheme
;; Version 4 - disallow "[" except for sole "[" alias of "test(1)"
;; Version 5 - cope with non-existent `default-directory'
;; Version 6 - fix perl package name pruning when a section number too
;; Version 7 - notice man-db index file changes to refresh page names
;; Version 8 - page name completion case-insensitive
;; Version 9 - disallow "/" for page name at point
;; Version 10 - disallow dash before hyphen like "X--\nY"
;; Version 11 - add emacs23 `boundaries' in completion
;; Version 12 - :: gets perl page name mangling
;; Version 13 - tie-in to completing-help.el for page descriptions
;; Version 14 - tie-in to icicles.el help, and some bug fixes
;; Version 15 - boundaries at perl "::" separator (emacs23)

;;; Code:

;;;###autoload (eval-after-load "man" '(require 'man-completion))

(require 'man)


;;----------------------------------------------------------------------------
;; xemacs21 lacking

;; no `eval-when-compile' on this fboundp because in xemacs 21.4.22
;; easy-mmode.el (which is define-minor-mode etc) rudely defines a
;; replace-regexp-in-string, so a compile-time test is unreliable
;;
;; [same in ffap-makefile-vars.el]
;;
(if (fboundp 'replace-regexp-in-string)
    ;; emacs21 up
    (eval-and-compile
      (defalias 'man-completion--replace-regexp-in-string
        'replace-regexp-in-string))

  ;; xemacs21
  (defun man-completion--replace-regexp-in-string
    (regexp rep string fixedcase literal)
    "`replace-regexp-in-string' made available in xemacs.
The FIXEDCASE argument is ignored, case is always fixed."
    (replace-in-string string regexp rep literal)))

;;----------------------------------------------------------------------------
;; emacs22 new stuff

;; `file-name-completion' gaining PRED argument in emacs22
(if (eval-when-compile
      (let ((func (symbol-function 'file-name-completion)))
        (and (subrp func)
             (if (eval-when-compile (fboundp 'subr-arity))
                 (equal '(2 . 2) (subr-arity func)) ;; emacs21
               (equal 2 (subr-max-args func))))))   ;; xemacs21

    ;; emacs21, xemacs21
    (defun man-completion--file-name-completion (file dir &optional pred)
      "`file-name-completion' with PRED argument like Emacs 22.
But PRED is ignored, which is not right, but might be close
enough for current uses."
      (file-name-completion file dir))

  ;; emacs22 up
  (eval-and-compile
    (defalias 'man-completion--file-name-completion
      'file-name-completion)))

(if (eval-when-compile (fboundp 'complete-with-action))
    ;; emacs22 (and in emacs23 recognising the "boundaries" thing)
    (eval-and-compile
      (defalias 'man-completion--complete-with-action
        'complete-with-action))

  ;; emacs21,xemacs21
  (defun man-completion--complete-with-action (action table string pred)
    (cond ((null action)
           (try-completion string table pred))
          ((eq action t)
           (all-completions string table pred))
          (t
           (eq t (try-completion string table pred))))))

;;----------------------------------------------------------------------------
;; variables

;;;###autoload
(defgroup man-completion nil "Man-Completion."
 :prefix "man-completion-"
 :group 'applications
 :link  '(url-link :tag "man-completion.el home page"
                   "http://user42.tuxfamily.org/man-completion/index.html"))

(defcustom man-completion-index-files
  '("/var/cache/man/index.db")
  "Files used by \"man\" for its apropos database.
man-completion notes the file times and if they change it re-runs
\"man\" for a new set of page names.  Usually this is when the
system administrator installs new pages.

The default is a single file \"/var/cache/man/index.db\", which
is what man-db (URL `http://man-db.nongnu.org/') uses.  (Are the
language specific sub-trees like \"/var/cache/man/pt_BR/index.db\"
needed too?)

It's fine if some (or all) the files here don't exist.
man-completion monitors the modtimes of those which exist, and
watches for the non-existent ones being created."

  ;; there's no "options" scheme for `repeat', otherwise might put some
  ;; likely files for various systems
  :group 'man-completion
  :type  '(repeat file))

(defcustom man-completion-at-point-functions nil
  "List of functions to modify `man-completion-at-point'.
Each function is called (FUNC STR) with the apparent page name.
FUNC can transform STR and return a new string which goes on to
the further functions, or it can return either nil or STR
unchanged if it's got nothing to apply.

Caution: This is slightly experimental.  Maybe page name
transformations could be better applied to the user-entered
string too, or instead."

  :group   'man-completion
  :type    'hook
  :options '(man-completion-transform-perl
             man-completion-transform-poco))


;;----------------------------------------------------------------------------
;; modifying the guessed page at point

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

These actions are only applied if STR has an upper case letter or
a \"::\", so that plain shell command pages are looked up without
loading the page name cache."

  (and (let ((case-fold-search nil))
         (string-match "[A-Z]\\|::" str))
       ;; don't change if STR is already an exact match
       (not (assoc str (man-completion-cache)))
       ;; prefix expansion first and if it produces a match then don't
       ;; need to prune
       (or (man-completion-perl-expand-prefix str)
           (man-completion-perl-prune-suffix str))))

(defun man-completion-perl-expand-prefix (str)
  "Expand a perl package name STR.
This is part of `man-completion-transform-perl'.
Return an expanded string, or nil if nothing found."

  (eval-and-compile (require 'cl))
  (let* ((case-fold-search nil)
         (matches (remove* (concat "\\(\\`\\|::\\)"
                                   (regexp-quote str) "\\'")
                           (man-completion-cache)
                           :key 'car
                           :test-not 'string-match)))
    (setq matches (mapcar (lambda (elem)
                            (man-completion-name-sans-section (car elem)))
                          matches))
    (setq matches (remove-duplicates matches :test 'string-equal))
    (and (= 1 (length matches))
         (car matches))))

(defun man-completion-perl-prune-suffix (str)
  "Prune a perl package name STR.
This is part of `man-completion-transform-perl'.
Return a pruned string, or nil if nothing found."

  ;; only act when there's a :: and when the whole thing isn't already good
  (when (and (string-match "::" str)
             (not (assoc str (man-completion-cache))))

    ;; something like "AptPkg::Cache(3pm)" won't be in the cache with its
    ;; 3pm suffix since it's the sole manpage of that name, so try first
    ;; "AptPkg::Cache"
    (setq str (man-completion-name-sans-section str))

    ;; stop at pruned STR found or STR set to nil when no more "::"s
    (while (and (not (assoc str (man-completion-cache)))
                (setq str (and (string-match "\\(.*\\)::" str)
                               (match-string 1 str)))))
    str))

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
;; man.el has `Man-name-regexp' and friends, but this pattern accepts
;; non-ascii, restricts punctuation, and does hyphenation better.

(defconst man-completion-hyphen-regexp
  (concat "[-"
          
          ;; xemacs21 doesn't have coding-system-p or decode-char, so use
          ;; coding-system-list and decode-coding-string (of course you need
          ;; mule-ucs to have utf-8 in xemacs21 at all though).
          ;;
          ;; other unicode like en-dash, em-dash or non-breaking hyphen
          ;; would be conceivable too, perhaps in rendered html output, but
          ;; leave them out until seen in practice.  Anything that's really
          ;; a hyphen probably ought to be U+2010 hyphen.
          ;;
          (and (memq 'utf-8 (coding-system-list))
               (let ((str (string #xE2 #x80 #x90))) ;; U+2010 HYPHEN
                 (if (eval-when-compile (fboundp 'string-make-unibyte))
                     (setq str (string-make-unibyte str)))
                 (decode-coding-string str 'utf-8)))

          ;; latin1 #xAD SOFT HYPHEN
          (decode-coding-string (string #xAD) 'iso-8859-1)

          "]" "\n[ \t]*")

  "Regexp for a hyphenated line break.
This is an internal part of `man-completion-at-point'.

groff spits out ascii, latin1 or unicode U+2010 dashes according
to its output mode.  groff breaks page names rather a lot in its
output, so recognising and collapsing them is important.")

(defconst man-completion-at-point-regexp
  ;;
  ;; `chars' is the permitted characters.  The aim is to make this tight to
  ;; avoid punctuation in text, or operators in program code.
  ;;
  ;;   * The syntax table definition of a word char or symbol char etc is
  ;;     not used, so as to be mode-independent.  It's not unusual to find
  ;;     man page names in comments which are not directly related to the
  ;;     programming language at hand.
  ;;
  ;;   * [:alnum:] includes non-ascii chars, so a word at point with
  ;;     accented chars is taken whole, not just an ascii part of it.
  ;;     Actual non-ascii page names will be unlikely, since there's so many
  ;;     ways for it to go wrong between filename coding, file content
  ;;     coding and emacs read coding.
  ;;
  ;;   * [:alnum:] in emacs23 only means word-syntax for non-ascii chars.
  ;;     That's close enough, though the intention is to be
  ;;     mode-independent, including syntax-table independent.
  ;;
  ;;   * [:alnum:] not available in xemacs21, hence the fallback to A-Z etc.
  ;;     What would be better for non-ascii there?
  ;;
  ;;   * "/" is disallowed since believe it doesn't normally occur, and
  ;;     disallowing it gets the the right page name from a perl POD
  ;;     L<pagename/section>.  The way man uses directories to hold pages
  ;;     will mean there can't be a "/" in the primary page name, though it
  ;;     might conceivably be in an alias shown in the NAME part.
  ;;
  ;; `word' is setup so
  ;;
  ;;   * "." and "-" are allowed in the middle of a page name, but not as
  ;;     the first or last char.  Disallowing a final dot helps a page name
  ;;     at the end of a sentence.
  ;;
  ;;   * "-" is not allowed before a hyphen sequence "-\n", so "foo--\nbar"
  ;;     is not considered a hyphenation.  This helps when the dashes on the
  ;;     previous line are a separator like a "--cut-here--".  groff doesn't
  ;;     hyphenate after a dash, unless you explicitly give it permission
  ;;     with \%, so a double dash shouldn't normally occur.
  ;;
  ;;   * "+" is allowed in the middle or at the end, but not at the start.
  ;;     "+" at the end is needed for say "g++" and other C++ stuff.
  ;;
  ;;   * "[" and "]" are disallowed like other parentheses, but there's a
  ;;     special case in `man-completion-at-point' for "[" alone, the shell
  ;;     "test" command.
  ;;
  ;;   * [:digit:] digits are allowed at the start of a page name.  This is
  ;;     unusual, but found in things like the Debian "822-date" program.
  ;;
  ;; `man-completion-at-point-regexp' is then setup so
  ;;
  ;;   * "::" is allowed for perl module names like B::Lint etc.  Each part
  ;;     must be a `word' per the rule above.  Don't think ":" occurs in a
  ;;     page name apart from doubled for perl, but maybe that could be
  ;;     loosened (which would simplify the regexp too).  A ":" at the end
  ;;     is disallowed though, since that'd most often be punctuation.
  ;;
  ;;   * A "::" is deliberately not matched at the end, for the benefit of
  ;;     perl code writing say Glib::Object:: for a class name.  In that
  ;;     case just Glib::Object is the page name.
  ;;
  ;;   * A section suffix like "chmod(2)" is matched only as a digit "(2)"
  ;;     or digit and alnums like "(3X)" or "(3ncurses)".  The match is kept
  ;;     tight so that on program code like "exit(errors ? 0 : 1)" it won't
  ;;     get the arg in parens, just the "exit".  Clearly this can be
  ;;     tricked by say "exit(1)" as code, but a space is not matched so GNU
  ;;     style "exit (1)" is ok.  It's expected that in comments or a
  ;;     document you won't put a space before a section number.
  ;;
  ;;     Alpha-only section IDs were, maybe still are, found on some
  ;;     non-free systems.  One system of evil memory, which best remain
  ;;     nameless, but which had letters S, C, and O in its name, had say
  ;;     "(S)" for system commands or something like that.  Really not too
  ;;     interested in doing anything for those.
  ;;
  (let* ((alnum  (if (string-match "[[:alnum:]]" "A") "[:alnum:]" "A-Za-z0-9"))
         (digit  (if (string-match "[[:digit:]]" "0") "[:digit:]" "0-9"))
         (chars  (concat alnum "_"))
         (first  (concat "[" chars ".]"))  ;; first, maybe only, char
         (last   (concat "[" chars "+]"))  ;; last char
         (middle (concat "[" chars ".+-]"))
         (nodash (concat "[" chars ".+]")) ;; middle without dash
         (hyphen man-completion-hyphen-regexp)
         (word   (concat first "\\("
                         "\\(" hyphen "\\)?"
                         "\\(" middle "*" nodash hyphen "\\)*"
                         middle "*" last
                         "\\)?")))
    (concat word
            "\\(::" word "\\)*"              ;; perl "::" module name parts
            "\\(([" digit "][" alnum "_]*)\\)?")) ;; optional section suffix

  "Regexp for a man page name at point.
This is an internal part of `man-completion-at-point'.")

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
                     (man-completion--replace-regexp-in-string
                      man-completion-hyphen-regexp "" (match-string 0) t t)))))
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
;; man page name cache

(defvar man-completion-cache 'uninitialized
  "An alist of available man pages for completion.
When not yet initialized it's the symbol `uninitialized' instead
of a list.  Function `man-completion-cache' initializes if not
already done.  Each entry is

    (NAME-STRING . nil)

or if descriptions have been requested then

    (NAME-STRING . DESCRIPTION-STRING)

For a plain page like \"cat\" there's two entries, the plain name
and with its section

    \"cat\"
    \"1 cat\"

If there's two pages with the same name in different sections
then parenthesized section number forms are included too, so as
to present those alternatives in completion.  The plain name is
also present so completion stops before the \"(\".  Eg.

    \"chmod\"
    \"chmod(1)\"
    \"chmod(2)\"
    \"1 chmod\"
    \"2 chmod\"

Emacs 22 allows a list of strings for completion as well as an
alist, but in Emacs 21 it must be an alist.  Making
man-completion-cache vary would be too tedious for
`man-completion-at-point-functions' which consult it, so it's
always an alist.")

(defvar man-completion-index-files-seen nil
  "Alist of modification times of `man-completion-index-files'.
This is the last seen times.  Currently each element is
    (FILENAME . MODTIME)
where MODTIME is a two-element list per `file-attributes'.")

(defun man-completion-index-files-changed-p ()
  "Check whether any `man-completion-index-files' have changed.
Return true if the files have changed.  The last noticed file
times are updated so this function returns true just once until
the files change again.

If a file has been added to `man-completion-index-files' since
the last check then currently it's treated as unchanged.
Normally you should set interesting `man-completion-index-files'
before doing things with `man-completion-cache'."

  (let (changed)
    (setq man-completion-index-files-seen
          (mapcar (lambda (filename)
                    (let ((modtime (nth 5 (file-attributes filename)))
                          (seen    (assoc filename
                                          man-completion-index-files-seen)))
                      (when (and seen
                                 (not (equal modtime (cdr seen))))
                        (setq changed t))
                      (cons filename modtime)))
                  man-completion-index-files))
    changed))

(defun man-completion-reset ()
  "Discard data cached for `man-completion-read'.
This can be used to get newly installed man pages recognised,
though if the `man-completion-index-files' scheme is working this
should happen automatically."
  (interactive)
  (setq man-completion-cache 'uninitialized))

(defun man-completion-cache (&optional want-desc)
  "Generate and return the `man-completion-cache' list.
If WANT-DESC is t then the list includes page descriptions (by is
re-loading if it doesn't already have descriptions)."

  ;; This code isn't blindingly fast if you've got a lot of man pages, but
  ;; the time goes in "man -k" and in reading its output into a buffer, so
  ;; not much can be done.
  ;;
  ;; The man-db system also offers its page database with "accessdb".
  ;; accessdb seems a touch quicker than "man -k", but the latter should be
  ;; adequate for now.  (The cutest thing might be direct dbm access to the
  ;; man-db database, if it can do partial key searches, and if you didn't
  ;; mind getting intimate with stuff normally internal to man-db.)

  ;; call `man-completion-index-files-changed-p' even when 'uninitialized,
  ;; so it records the current times of the files, for future comparison
  (when (or (man-completion-index-files-changed-p)
            (eq 'uninitialized man-completion-cache)
            (and want-desc
                 (not (cdar man-completion-cache))))
    (message "Building man page completions ...")
    (with-temp-buffer
      (setq default-directory "/") ;; in case inherited doesn't exist

      ;; "man -k" truncates long page names to the tty width, so set COLUMNS
      ;; to avoid that; and force process-connection-type to a pipe so man
      ;; can't maybe get the width on the pseudo-tty TIOCGWINSZ instead of
      ;; COLUMNS.
      ;;
      ;; Old man.el in xemacs has `Manual-program' in man.el as well as
      ;; `manual-program' in paths.el.  Which has greater virtue?
      ;; Just `manual-program' is easier because that's the one in emacs.
      ;;
      (let ((process-environment (copy-sequence process-environment))
            (process-connection-type nil))
        (setenv "COLUMNS" "999")
        (call-process manual-program nil t nil "-k" ""))

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
      (let ((ret (if want-desc
                     '(("-l " . "run man on a file")
                       ("-k " . "search apropos database"))
                   '(("-l ")
                     ("-k "))))
            prev prevsect prevdesc)

        ;; "man -k" gives lines like
        ;;     cat (1)              - concatenate files and print ...
        ;; or for an "alias"
        ;;     boot-scripts (7) [boot] - General description of boot sequence
        ;;
        ;; ".*?" is the page name, the first parens "(foo)" is the section,
        ;; then skip "[foo]" real page and " - " to get to the description
        ;; part.
        ;;
        (goto-char (point-min))
        (while (re-search-forward "\
^\\(.*?\\)\
 (\\([^)]+?\\))\
\\(?: \\[[^]*]\\)?\
\\(?: +- \\)?\
\\(.*\\)" nil t)
          (let ((name (match-string 1))
                (sect (match-string 2))
                (desc (and want-desc (match-string 3))))

            ;; If previous name was the same then put it with its prevsect
            ;; like "chmod(1)" and also the present one like "chmod(2)".
            (if (equal name prev)
                (progn
                  (setq ret (cons (cons (concat name "(" sect ")")
                                        desc)
                                  ret))
                  (when prevsect
                    (setq ret (cons (cons (concat prev "(" prevsect ")")
                                          prevdesc)
                                    ret))
                    (setq prevsect nil)))

              ;; If previous name different then just a solitary entry for
              ;; this one, like "chmod".
              (setq ret      (cons (cons name desc) ret)
                    prev     name
                    prevsect sect
                    prevdesc desc))

            ;; always an entry like "1 chmod" or "2 chmod" with the sect
            (setq ret (cons (cons (concat sect " " name) desc)
                            ret))))

        (setq man-completion-cache (nreverse ret))))
    (message ""))
  man-completion-cache)


;;----------------------------------------------------------------------------
;; page name completion

(defconst man-completion-filename-regexp
  "\\`\\(-l \\)\\|\\`[./]"
  "Pattern for a filename in `man-completion-read'.
Currently this means starting with \"/\" or \".\" or \"-l \".")

(defun man-completion-handler (str pred action)
  "Perform completion for `man-completion-read'."
  (if (string-match man-completion-filename-regexp str)
      ;; filename
      (let* ((before (or (match-string 1 str) ""))
             (fname  (substring str (or (match-end 1) 0)))
             (dir    (or (file-name-directory fname) default-directory))
             (part   (file-name-nondirectory fname))

             (completion-ignore-case
              ;; xemacs21 doesn't have `read-file-name-completion-ignore-case'
              (if (boundp 'read-file-name-completion-ignore-case)
                  read-file-name-completion-ignore-case
                completion-ignore-case)))

        (cond ((eq (car-safe action) 'boundaries) ;; emacs23
               ;; boundaries at "/" like `completion-file-name-table', but
               ;; skipping any "-l " at the start of STR
               (let* ((suffix (cdr action))
                      (start  (+ (length before)
                                 (length (file-name-directory fname))))
                      (end    (string-match "/" suffix)))
                 (cons 'boundaries (cons start end))))
              ((null action)
               (let ((ret (man-completion--file-name-completion part dir pred)))
                 (if (stringp ret)
                     (concat before dir ret)
                   ret)))
              ((eq action t)
               ;; FIXME: pred not applied here.  There's code in
               ;; `completion-file-name-table', coming in emacs 23.2 or
               ;; whatever, for that, but don't think it matters for plain
               ;; completing-read as used below.
               (mapcar (lambda (s) (concat before dir s))
                       (file-name-all-completions part dir)))
              (t ;; 'lambda for test-completion
               (file-exists-p fname))))

    ;; pagename
    (cond ((eq (car-safe action) 'boundaries) ;; emacs23
           ;; Boundaries at perl "::" class name separator.  Won't hurt
           ;; anything else, and perl is a big user of man pages.
           (let* ((suffix (cdr action))
                  ;; after last "::", or after start of string
                  ;; (page names don't have newlines, so ".*" is ok)
                  (start  (if (string-match ".*::" str) (match-end 0) 0))
                  ;; first "::", or end of string
                  (end    (string-match "::\\|\\'" suffix)))
             (cons 'boundaries (cons start end))))
          (t
           (man-completion--complete-with-action action (man-completion-cache)
                                                 str pred)))))

(defun man-completion-read ()
  "Read the name of a man page.
A page name at point is offered as a default, and completions
come from the pages reported by \"man -k\" (its \"apropos\"
database).

A filename can also be entered too as any of the following, with
filename completion.

    ./foo
    /some/dir/foo
    -l foo

An apropos query can be entered too as \"-k foo\" like the
\"man\" command line.  This is a normal feature of M-x man,
although as Emacs 23 if there's no matches the error message is
\"Can't find the -k foo manpage\", which might make you think it
tried to get a page called \"-k\" or that man-completion has
upset it, but it just means no matches.

Page name completion is case-insensitive because \"man\" is
case-insensitive and because it helps on odd combinations of
upper/lower in page names.  Filename case sensitivity follows
`read-file-name-completion-ignore-case' the same as
`read-file-name' does.

If the default page name offered is not quite right remember
the usual `\\<minibuffer-local-map>\\[next-history-element]' puts it in the minibuffer for you to edit.

If `completing-help-mode' is enabled then page descriptions are
shown in the completions list (with `\\<minibuffer-local-completion-map>\\[minibuffer-completion-help]' etc).

The man-completion home page is
URL `http://user42.tuxfamily.org/man-completion/index.html'"

  ;; completion-ignore-case is set here rather than within the handler with
  ;; the idea a C-t could toggle sensitivity by changing the variable.  But
  ;; perhaps it would better operate on a man-completion-ignore-case than
  ;; the global.
  ;;
  (let* ((completion-ignore-case t)
         (icicle-candidate-help-fn 'man-completion-icicle-help)
         (default  (man-completion-at-point))
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

;; emacs
(defadvice man (before man-completion activate)
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))

;; xemacs
(defadvice manual-entry (before man-completion activate)
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))


;;----------------------------------------------------------------------------
;; completing-help.el tie-in

(defvar man-completion-completing-help-group
  '(:predicate man-completion-completing-help-p
    :get       man-completion-completing-help-get
    :info-head " - "
    :info-tail "")
  "`completing-help-mode' configuration for `man-completion-read'.")

(defun man-completion-completing-help-p ()
  "Return non-nil if within a `man-completion-read'."
  (eq minibuffer-completion-table 'man-completion-handler))

(defun man-completion-completing-help-get (str)
  "Return man page description for STR, or nil if unknown.
If STR is one of the filename forms supported by
`man-completion-read' then the return is always nil."
  (and (not (string-match man-completion-filename-regexp str))
       (cdr (assoc str (man-completion-cache t)))))
    
(eval-after-load "completing-help"
  '(add-to-list 'completing-help-groups 'man-completion-completing-help-group))

;;----------------------------------------------------------------------------
;; icicles.el tie-in

(defun man-completion-icicle-help (str)
  "Display help for STR in `icicles-mode'.
Man page descriptions are shown with
`icicle-msg-maybe-in-minibuffer' (because they're just a single
line).  Files are shown with `icicle-describe-file'."
  (if (string-match man-completion-filename-regexp str)
      (let ((filename (substring str (or (match-end 1) 0))))
        (unless (equal "" filename)
          ;; not sure it's terrific on a non-existent file, but leave
          ;; that to be handled by icicles
          (icicle-describe-file filename)))
    (let ((desc (cdr (assoc str (man-completion-cache t)))))
      (if desc
          (icicle-msg-maybe-in-minibuffer "%s" desc)))))


(provide 'man-completion)

;;; man-completion.el ends here
