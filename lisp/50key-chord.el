;;; 50key-chord.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

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

;;

;;; Code:
;; notice: toggle input method will cause key-chord work abnormally

;;; banish the shift key
;; http://endlessparentheses.com/banishing-the-shift-key-with-key-chord-in-emacs.html
(defconst banish-shift-key-combo
  '(("`j" . "~")
    ("1j" . "!")
    ("2j" . "@")
    ("3j" . "#")
    ("4j" . "$")
    ("5j" . "%")
    ("6f" . "^")
    ("7f" . "&")
    ("8f" . "*")
    ("9f" . "(")
    ("0f" . ")")
    ("-f" . "_")
    ("=f" . "+")
    (";f" . ":")
    ("[f" . "{")
    ("]f" . "}")
    ("/j" . "?")
    ("\\j" . "|")))

;; maybe try to bind a key chord to a key map is a good idea
(with-eval-after-load "key-chord"
  (dolist (combo banish-shift-key-combo)
    (key-chord-define-global (car combo) (cdr combo)))

  (key-chord-define-global "''" 'paloryemacs/apostrophe-key-chord)
  (key-chord-define-global ";b" 'switch-to-buffer)
  ;; (key-chord-define-global ";r" 'helm-resume)
  ;; (key-chord-define-global ";s" 'helm-occur)
  ;; (key-chord-define-global ";f" 'helm-for-files)
  (key-chord-define-global ";d" 'dired-jump-other-window)

  (key-chord-define-global "bf" 'ibuffer)
  (key-chord-define-global "cx" ctl-x-map)

  ;; (key-chord-define-global "jk" 'company-complete)

  ;; (key-chord-define-global "jc" 'avy-goto-char)
  (key-chord-define-global "jc" 'avy-goto-char-timer)
  (key-chord-define-global "jw" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)

  (key-chord-define-global "/s" 'save-buffer)
  ;; (key-chord-define-global "##" 'server-edit)

  (key-chord-define-global "hd" 'helm-dash)

  ;; ";g" -> goto-last-change
  ;; (key-chord-define-global ";g" 'magit-status)
  ;; (key-chord-define-global "ms" 'mark-sexp)
  (key-chord-define-global ";w" 'paloryemacs/w3m-switch-to-buffer)

  (key-chord-define-global "YY" 'browse-kill-ring)


  (key-chord-define-global ",r" 'paloryemacs/recursive-edit-save-window-config)
  (key-chord-define-global ",c" 'org-capture)
  (key-chord-define-global "VV" 'other-window))


(provide '50key-chord)
;;; 50key-chord.el ends here
