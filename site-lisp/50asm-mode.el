

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'asm-mode-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (make-local-variable 'compile-command)
;; 	     (setq compile-command
;; 		 (concat "as "
;; 			 (file-name-sans-extension buffer-file-name))))))

;; These should be added with hooks.

(add-hook 'asm-mode-hook
          (function
           (lambda ()
            ;; (setq tab-width 4)
            ;; (setq indent-tabs-mode nil)
            (setq blink-matching-paren nil)
            ;; (setq asm-style "nasm")    ; for use with my hacked asm-mode.el
            ;; Run the assembler directly, useful for quick test code.
            ;; (local-set-key (kbd "<f8>")
            ;; (lambda ()
            ;; (interactive)
            ;; (require 'compile)
            ;; (compile-internal (read-from-minibuffer
            ;; "Assamble: "
            ;; (format "%s %s "
            ;; AS
            ;; buffer-file-name))
            ;; "No more errors")))
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun asm-file ( filename )
;;   "Calls the as6811 assembler on a named file.
;; asm-file FILENAME - can be called interactively, whereupon the user
;;                     is prompted for the name of the file."
;;   (interactive "fEnter name of file to assemble: ")
;;   (compile (concat "asm " filename))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun asm ( )
;;   "Calls the as6811 assembler on the file associated with the current buffer."
;;   (interactive)
;;   (asm-file (buffer-file-name))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-create-file (filename author purpose)
  "Function to create an as6811 file with a pre-inserted template.
   asm-create-file FILENAME AUTHOR PURPOSE"
  (interactive "FName of the new file: \nsAuthor name: \nsBrief description: ")
  ;; Create the file if it does not exist.
  (if (file-exists-p filename)
      (message "File already exists.  Try asm-insert template instead.")
      (find-file filename)
      ;; Do the rest of the work in asm-insert-template.
      (asm-insert-template author purpose))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-insert-template (author purpose)
  "Function to insert a conditional assembly template into the current buffer.
   Interactively prompts for author name and a purpose of the file.
   asm-insert-template AUTHOR PURPOSE"
  (interactive "sAuthor name: \nsBrief description: ")
  ;; Insert the skeleton.
  ;; First put in a header.
  (asm-insert-header author purpose)
  ;; Add in the rest of the stuff to create a main assembly file.
  (insert (concat
           "EEPROM  =       0               ; Conditional assembly flag.\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           "                .area   "
           (file-name-nondirectory (file-name-sans-extension buffer-file-name))
           " (ABS)\n"
           "                .module "
           (file-name-nondirectory (file-name-sans-extension buffer-file-name))
           "\n\n"
           "        .if EEPROM\n"
           "                .org 0xC000\n"
           "Startup:        lds   #0x00FF   ; or 0x7FFF for external RAM.\n"
           "        .else\n"
           "                .org 0x1040\n"
           "        .endif\n\n"
           "Main:   \n\n\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           ";;; Application-specific subroutines.\n"
           ";;;--------------------------------------------------------"
           "-----------\n\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           ";;; Included files.\n"
           ";;;--------------------------------------------------------"
           "-----------\n\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           ";;; Constants (use only .byte, .word, .db, .dw, .ascii, "
           " and .asciz).\n"
           ";;;--------------------------------------------------------"
           "-----------\n\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           ";;; Variables (use only .ds).\n"
           ";;;--------------------------------------------------------"
           "-----------\n\n\n"
           ";;;--------------------------------------------------------"
           "-----------\n"
           ";;; Vectors.\n"
           ";;;--------------------------------------------------------"
           "-----------\n\n"
           "        .if EEPROM\n"
           "                .org 0xFFFE\n"
           "                .dw  Startup\n"
           "        .endif\n"
           ))
  ;; Move the cursor to the Main: label.
  (goto-char (point-min))
  (search-forward "Main:")
  (end-of-line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-insert-header (author purpose)
  "Generates an assembly file header.
   asm-insert-header AUTHOR PURPOSE
   can only be called from a buffer which is visiting a file."
  (interactive "sAuthor name: \nsBrief description: ")
  (goto-char (point-min))
  (insert (concat ".title " (file-name-nondirectory buffer-file-name)
                  " by " author "\n\n"
                  ";;;----------------------------------------------------"
                  "---------------\n"
                  ";;; File name:   "
                  (file-name-nondirectory buffer-file-name)
                  "\n"
                  ";;; Author:      " author "\n"
                  ";;; Purpose:     " purpose "\n"
                  ";;; Creation:    \n"
                  ";;;----------------------------------------------------"
                  "---------------\n\n"
                  ";;;----------------------------------------------------"
                  "---------------\n"
                  ";;; Equates.\n"
                  ";;;----------------------------------------------------"
                  "---------------\n"))
  ;; Establish the creation date.
  (goto-char (point-min))
  (search-forward "Creation:")
  (end-of-line)
  (shell-command "date" t)
  (end-of-line)
  (delete-char 1)
  (goto-char (point-max))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-create-library (filename author purpose)
  "Function to create an as6811 library file with a pre-inserted template.
   asm-create-library FILENAME AUTHOR PURPOSE"
  (interactive "FName of the new library file: \nsAuthor name: \nsBrief description: ")
  ;; Create the file if it does not exist.
  (if (file-exists-p filename)
      (message "File already exists.  Try asm-insert template instead.")
      (find-file filename)
      ;; Do the rest of the work in asm-insert-template.
      (asm-insert-header author purpose)
      (goto-char (point-max)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-insert-subroutine (subname purpose requires returns)
  "Function to create an as6811 library file with a pre-inserted template.
asm-create-library FILENAME AUTHOR PURPOSE"
  (interactive "sSubroutine name: \nsBrief description: \nsRequires (inputs): \nsReturns (outputs): ")
  (beginning-of-line)
  (insert (concat ";;; ***************************************************"
                  "***************\n"
                  ";;; Subroutine:     " subname "\n"
                  ";;; Purpose:        " purpose "\n"
                  ";;; Requires:       " requires "\n"
                  ";;; Returns:        " returns "\n"
                  ";;; Regs affected:  none.\n"
                  ";;; ***************************************************"
                  "***************\n"
                  subname ":\n\n"
                  "        rts"))
                                        ; Move back one line and tab over.
  (forward-line -1)
  (insert "        ")

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-ta-file ( fname )
  "Assembly mode ASCII transfer.  Used to send .s19 files to a microboard.
Sends the .s19 file associated with the current assembly file through the
serial port."
  (interactive "fEnter name of file to send: ")
  (let ((filename (file-name-sans-extension fname)))
    (message "Sending %s.s19 in ASCII mode." filename)
    (shell-command (concat "ta " filename
                           ".s19 &"))
    (switch-to-buffer "*terminal*"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asm-ta ()
  "Assembly mode ASCII transfer.  Used to send .s19 files to a microboard.
Sends the .s19 file associated with the current assembly file through the
serial port."
  (interactive)
  (asm-ta-file (file-name-sans-extension (buffer-file-name)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings for asm-mode.
(add-hook 'asm-mode-hook
          '(lambda ()
            (define-key asm-mode-map "\C-cf" 'asm-create-file)
            (define-key asm-mode-map "\C-cl" 'asm-create-library)
            (define-key asm-mode-map "\C-c\C-a" 'asm-file)
            (define-key asm-mode-map "\C-ca" 'asm)
            (define-key asm-mode-map "\C-cs" 'asm-insert-subroutine)
            (define-key asm-mode-map "\C-ct" 'asm-ta)
            (define-key asm-mode-map "\C-c\C-t" 'asm-ta-file)
                                        ; (define-key asm-mode-map [f1] 'asm)
                                        ; (define-key asm-mode-map [f2] 'next-error)
                                        ; (define-key asm-mode-map [f3] 'asm-ta)
                                        ;     (define-key asm-mode-map [f4] 'asm-insert-subroutine)

            )
          )

