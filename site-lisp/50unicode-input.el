;;; tsp-unicode-input.el ---

;;;; INSERT UNICODE AND MATH CHARS
(global-set-key (kbd "s-i s-i") 'open-unicode-template)
(global-set-key (kbd "s-i <up>") (lambda () (interactive) (insert "↑")))
(global-set-key (kbd "s-i <down>") (lambda () (interactive) (insert "↓")))
(global-set-key (kbd "s-i <left>") (lambda () (interactive) (insert "←")))
(global-set-key (kbd "s-i <right>") (lambda () (interactive) (insert "→")))
(global-set-key (kbd "s-i <kp-1>") (lambda () (interactive) (insert "↙")))
(global-set-key (kbd "s-i <kp-3>") (lambda () (interactive) (insert "↘")))
(global-set-key (kbd "s-i <kp-7>") (lambda () (interactive) (insert "↖")))
(global-set-key (kbd "s-i <kp-9>") (lambda () (interactive) (insert "↗")))
(global-set-key (kbd "s-i <kp-2>") (lambda () (interactive) (insert "⇓")))
(global-set-key (kbd "s-i <kp-4>") (lambda () (interactive) (insert "⇐")))
(global-set-key (kbd "s-i <kp-6>") (lambda () (interactive) (insert "⇒")))
(global-set-key (kbd "s-i <kp-8>") (lambda () (interactive) (insert "⇑")))
(global-set-key (kbd "s-i <kp-add>") (lambda () (interactive) (insert "⊕")))
(global-set-key (kbd "s-i <kp-subtract>") (lambda () (interactive) (insert "⊖")))
(global-set-key (kbd "s-i <kp-multiply>") (lambda () (interactive) (insert "×")))
(global-set-key (kbd "s-i *") (lambda () (interactive) (insert "⊗")))

(global-set-key (kbd "s-i Z") (lambda () (interactive) (insert "ℤ")))
(global-set-key (kbd "s-i Q") (lambda () (interactive) (insert "ℚ")))
(global-set-key (kbd "s-i R") (lambda () (interactive) (insert "ℝ")))
(global-set-key (kbd "s-i C") (lambda () (interactive) (insert "ℂ")))

(global-set-key (kbd "s-i a") (lambda () (interactive) (insert "α")))
(global-set-key (kbd "s-i b") (lambda () (interactive) (insert "β")))
(global-set-key (kbd "s-i g") (lambda () (interactive) (insert "γ")))
(global-set-key (kbd "s-i t") (lambda () (interactive) (insert "θ")))
(global-set-key (kbd "s-i l") (lambda () (interactive) (insert "λ")))

(global-set-key (kbd "s-i A") (lambda () (interactive) (insert "∀")))
(global-set-key (kbd "s-i E") (lambda () (interactive) (insert "∃")))
(global-set-key (kbd "s-i ^") (lambda () (interactive) (insert "∧")))
(global-set-key (kbd "s-i 6") (lambda () (interactive) (insert "∨")))
(global-set-key (kbd "s-i !") (lambda () (interactive) (insert "¬")))
(global-set-key (kbd "s-i =") (lambda () (interactive) (insert "≡")))
(global-set-key (kbd "s-i .") (lambda () (interactive) (insert "∎")))



(defun unicode-insert (char)
  "Read a unicode code point and insert said character.
Input uses `read-quoted-char-radix'.  If you want to copy
the values from the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (ucs-insert char))

(defun open-unicode-template ()
  (interactive)
  (find-file "~/.emacs.d/unicode_template.txt"))



(provide '50unicode-input)