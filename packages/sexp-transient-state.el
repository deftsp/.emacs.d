;;; sexp-transient-state.el --- An hydra binding to edit sexp

;; Copyright (C) 2020 Shihpin Tseng
;;
;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: evil smartparens sexp lisp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; hydra binding for sexp

;; inspired by evil-lisp-state from Spacemacs

;;; Code:

(require 'evil)
(require 'smartparens)
(require 'hydra)

(defun evil-lisp-state-escape-command (command)
  "Wrap COMMAND to escape to normal state before executing COMMAND."
  `(lambda ()
     (interactive)
     (evil-normal-state)
     (call-interactively ',command)))

(defhydra sexp-transient-state (:hint nil)
  "
 [1..9] digit-argument

 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_0_] beginning  [_n_] down      [_S_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_$_] end        [_N_] bw down   [_B_] bw barf    [_w_]   unwrap        [_s_] splice  [_a_] absorb      [_C_] change outer
 [_L_] forward    [_u_] up        [_s_] slurp      [_W_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_H_] backward   [_U_] bw up     [_b_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_c_] convolute   [_K_] bw kill       [_q_] quit "
  ;; digit-argument
  ("1"    digit-argument)
  ("2"    digit-argument)
  ("3"    digit-argument)
  ("4"    digit-argument)
  ("5"    digit-argument)
  ("6"    digit-argument)
  ("7"    digit-argument)
  ("8"    digit-argument)
  ("9"    digit-argument)

  ("`k"   sp-kill-hybrid-sexp)
  ("`p"   sp-push-hybrid-sexp)
  ("`s"   sp-slurp-hybrid-sexp)
  ("`t"   sp-transpose-hybrid-sexp)



  ;; Moving
  ("0"    sts/beginning-of-sexp)
  ("$"    sp-end-of-sexp)
  ("h"    sp-backward-symbol)
  ("H"    sp-backward-sexp)
  ("L"    sp-forward-sexp)
  ("l"    sts/sp-forward-symbol)
  ("n"    sp-down-sexp)
  ("N"    sp-backward-down-sexp)
  ("u"    sp-up-sexp)
  ("U"    sp-backward-up-sexp)

  ;; Slurping & barfing
  ("s"    sp-forward-slurp-sexp )
  ("b"    sp-forward-barf-sexp)
  ("S"    sp-backward-slurp-sexp)
  ("B"    sp-backward-barf-sexp)


  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("w" sp-unwrap-sexp)
  ("W" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("a" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ;; ("c"    sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("y" sp-copy-sexp)

  ("ds"   sp-kill-symbol)
  ("Ds"   sp-backward-kill-symbol)
  ("dw"   sp-kill-word)
  ("Dw"   sp-backward-kill-word)
  ("dx"   sp-kill-sexp)
  ("Dx"   sp-backward-kill-sexp)
  ("e"    sp-splice-sexp-killing-forward)
  ("E"    sp-splice-sexp-killing-backward)

  ;; quit
  ("<escape>" nil)
  ("q" nil)
  ("g" nil))



(defun sts/forward-symbol (&optional arg)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (if (sts/lisp*-mode-p)
      (let ((n (if (char-equal (char-after) ?\() 1 2)))
        (sp-forward-symbol (+ (if arg arg 0) n))
        (sp-backward-symbol))
    (sp-forward-symbol)))

(defun sts/lisp*-mode-p ()
  (member major-mode
          '(emacs-lisp-mode
            clojure-mode
            racket-mode)))

(defun sts/beginning-of-sexp (&optional arg)
  "Go to the beginning of current s-exp"
  (interactive "P")
  (sp-beginning-of-sexp)
  (when (sts/lisp*-mode-p)
    (evil-backward-char)))


(provide 'sexp-transient-state)
