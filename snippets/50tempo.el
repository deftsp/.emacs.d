;;; tempo-mode

(use-package tempo
  :defer t
  :init
  (setq tempo-interactive t)
  :config
  ;; This is a way to hook tempo into cc-mode
  (defvar c-tempo-tags nil
    "Tempo tags for C mode")
  (defvar c++-tempo-tags nil
    "Tempo tags for C++ mode")

;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
  (add-hook 'c-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c TAB") 'tempo-complete-tag)
               (tempo-use-tag-list 'c-tempo-tags)))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c TAB") 'tempo-complete-tag)
               (tempo-use-tag-list 'c-tempo-tags)
               (tempo-use-tag-list 'c++-tempo-tags)))

;;; Preprocessor Templates (appended to c-tempo-tags)
  (tempo-define-template "c-include"
                         '("include <" r ".h>" > n)
                         "include"
                         "Insert a #include <> statement"
                         'c-tempo-tags)

  (tempo-define-template "c-ifdef"
                         '("ifdef " (p "ifdef-clause: " clause) > n> p n
                           "#else /* !(" (s clause) ") */" n> p n
                           "#endif /* " (s clause)" */" n>)
                         "ifdef"
                         "Insert a #ifdef #else #endif statement"
                         'c-tempo-tags)

  (tempo-define-template "c-ifndef"
                         '("ifndef " (p "ifndef-clause: " clause) > n
                           "#define " (s clause) n> p n
                           "#endif /* " (s clause)" */" n>)
                         "ifndef"
                         "Insert a #ifndef #define #endif statement"
                         'c-tempo-tags)
;;; C-Mode Templates

  (tempo-define-template "c-if"
                         '(> "if (" (p "if-clause: " clause) ") {" > n>
                             > r n
                             "}" > n>)
                         "if"
                         "Insert a C if statement"
                         'c-tempo-tags)

  (tempo-define-template "c-else"
                         '(> "else {"  > n>
                             > r n
                             "}" > n>)
                         "else"
                         "Insert a C else statement"
                         'c-tempo-tags)

  (tempo-define-template "c-if-else"
                         '(> "if (" (p "if-clause: " clause) ") {" > n
                             > r n
                             "} else {" > n>
                             > r n
                             "}" > n>)
                         "ifelse"
                         "Insert a C if else statement"
                         'c-tempo-tags)

  (tempo-define-template "c-while"
                         '(> "while (" (p "while-clause: " clause) ") {" > n
                             > r n
                             "}" > n>)
                         "while"
                         "Insert a C while statement"
                         'c-tempo-tags)

  (tempo-define-template "c-for"
                         '(> "for (" (p "for-clause: " clause) ") {" > n
                             > r n
                             "}" > n>)
                         "for"
                         "Insert a C for statement"
                         'c-tempo-tags)

  (tempo-define-template "c-for-i"
                         '(> "for (" (p "variable: " var) " = 0; " (s var)
                             " < "(p "upper bound: " ub)"; " (s var) "++) {" > n
                             > r n
                             "}" > n>)
                         "fori"
                         "Insert a C for loop: for(x = 0; x < ..; x++)"
                         'c-tempo-tags)

  (tempo-define-template "c-main"
                         '(> "int" > n
                             > "main(int argc, char *argv[]) {" > n>
                             > r n
                             > "return 0 ;" n>
                             > "}" > n>)
                         "main"
                         "Insert a C main statement"
                         'c-tempo-tags)

  (tempo-define-template "c-if-malloc"
                         '(> (p "variable: " var) " = ("
                             (p "type: " type) " *) malloc (sizeof(" (s type)
                             ") * " (p "nitems: " nitems) ") ;" n>
                             > "if (" (s var) " == NULL)" n>
                             > "error_exit (\"" (buffer-name) ": " r ": Failed to malloc() " (s var) " \") ;" n>)
                         "ifmalloc"
                         "Insert a C if (malloc...) statement"
                         'c-tempo-tags)

  (tempo-define-template "c-if-calloc"
                         '(> (p "variable: " var) " = ("
                             (p "type: " type) " *) calloc (sizeof(" (s type)
                             "), " (p "nitems: " nitems) ") ;" n>
                             > "if (" (s var) " == NULL)" n>
                             > "error_exit (\"" (buffer-name) ": " r ": Failed to calloc() " (s var) " \") ;" n>)
                         "ifcalloc"
                         "Insert a C if (calloc...) statement"
                         'c-tempo-tags)

  (tempo-define-template "c-switch"
                         '(> "switch (" (p "switch-condition: " clause) ") {" > n>
                             "case " (p "first value: ") ":" > n> p n
                             "break;" > n> p n
                             "default:" > n> p n
                             "break;" > n
                             "}" > n>)
                         "switch"
                         "Insert a C switch statement"
                         'c-tempo-tags)

  (tempo-define-template "c-case"
                         '(n "case " (p "value: ") ":" > n> p n
                             "break;" > n> p)
                         "case"
                         "Insert a C case statement"
                         'c-tempo-tags)
  (tempo-define-template "c-debug-pf"
                         '(> "printf(\"%s(%d):%s:" r "\\n\", __FILE__, __LINE__, __PRETTY_FUNCTION__);" > n>)
                         "debug-pf"
                         "Insert a printf statement with info on location"
                         'c-tempo-tags)

  (tempo-define-template "c++-class"
                         '("class " (p "classname: " class) p > n>
                           " {" > n
                           "public:" > n
                           "" > n
                           "protected:" > n
                           "" > n
                           "private:" > n
                           "" > n
                           "};" > n)
                         "class"
                         "Insert a class skeleton"
                         'c++-tempo-tags))


;; Completing-Read Prompts
;; Sometimes it’s desirable to offer a list of completions when being prompted. Here’s a way to do it:
;; (defun tempo-complete (prompt completions match-required
;;                    &optional save-name no-insert)
;;   "Do whatever `tempo-insert-prompt' does, but use completing-read."
;;   (flet ((read-string (prompt)
;;            (completing-read prompt completions match-required)))
;;     (tempo-insert-prompt prompt save-name no-insert)))

;; (tempo-define-template "begin-environment"
;;                        '("\\begin{"
;;                          '(tempo-complete "Environment: " (LaTeX-environment-list) nil 'environment)
;;                          "}" > n>
;;                          r> n>
;;                          "\\end{" (s environment) "}" > n))


(provide '50tempo)
