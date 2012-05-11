;;; 50dictionary.el ---
;;
;; Description:
;; Author: Shihpin Tseng <detfsp@gmail.com>
;; Created: Thu Sep 13 14:17:37 2007
;; Version:

;;; keys
;; (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words)

(global-set-key (kbd "C-c d d") 'sdcv-search)
(global-set-key (kbd "C-c d f") 'dictionary-lookup-definition)
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d m") 'dictionary-match-words)
(global-set-key (kbd "C-c d p") 'sdcv-with-tooltip)
(global-set-key (kbd "C-c d w") 'wordnet-search)


(autoload 'sdcv-search "sdcv-mode" "Prompt for a word to search through sdcv." t)

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(unless (boundp 'running-xemacs)
  (autoload 'global-dictionary-tooltip-mode "dictionary"
    "Enable/disable dictionary-tooltip-mode for all buffers" t))

;; (load "dictionary-init")


;; search online
;; (setq dictionary-proxy-port 31280
;;       dictionary-proxy-server "sc.net9.org"
;;       dictionary-use-http-proxy t)


;; search at home
(setq dictionary-server "localhost"
      dictionary-tooltip-dictionary "wn"
      dictionary-default-dictionary "*")

;; for dictionary tooltip mode
;; choose the dictionary: "wn" for WordNet
;; "web1913" for Webster's Revised Unabridged Dictionary(1913)
;; so on

(setq global-dictionary-tooltip-mode nil
      dictionary-tooltip-mode nil)

;; 设定中文词典的解码

;;发现新版的 dictd 把它全都以 UTF-8 编码方式送出来. 于是 .emacs 里修改一下:
(eval-after-load "dictionary"
  '(setq dictionary-coding-systems-for-dictionaries
    (append '(("moecomp" . utf-8)
              ("netterm" . utf-8)
              ("pydict"  . utf-8)
              ("cedict"  . utf-8)
              ("stardic" . utf-8)
              ("cpatch"  . utf-8)
              ("xdict"   . utf-8)
              ("cdict"   . gbk))
     dictionary-coding-systems-for-dictionaries)))

(defun tsp-dictionary-next-dictionary ()
  (interactive)
  (end-of-line)
  (search-forward-regexp "^From" nil t)
  (beginning-of-line))

(defun tsp-dictionary-prev-dictionary ()
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^From" nil t)
  (beginning-of-line))

(defun tsp-dictionary-mode-hook ()
  ;; faces
  (set-face-foreground 'dictionary-word-entry-face "magenta")

  (define-key dictionary-mode-map (kbd "<backtab>") 'dictionary-prev-link)
  (define-key dictionary-mode-map (kbd "n") 'tsp-dictionary-next-dictionary)
  (define-key dictionary-mode-map (kbd "p") 'tsp-dictionary-prev-dictionary))

(add-hook 'dictionary-mode-hook 'tsp-dictionary-mode-hook)

;;;; wordnet
;; -------------
(condition-case nil
    (require 'wordnet)
  (error nil))

;;;; edict
;; -------

;; FIXME
;; (require 'edict)
(defun edict-display (key-list match-list)
  "Edict-display displayes the strings in a separate window that is
not selected."
  (let* ((text-window (get-buffer-window (current-buffer)))
         (edict-window (get-buffer-window edict-match-buffer))
         ;; We have available some of this window's height plus any we've already
         ;; already gotten.
         (avail-height (+ (window-height text-window)
                          (if edict-window
                              (window-height edict-window)
                              0)))
         ;; We limit the height to half of what's available, but no more than we need,
         ;; and no less than window-min-height.  We must remember to include 1 line for
         ;; the mode-line in our minimum figure.
         (height (min (max window-min-height (+ (length match-list) 1))
                      (/ avail-height 2)))
         (transpose-window-p (not edict-window)))
    (if (not edict-window)
        (progn
          ;; We don't have a window, so remember our existing configuration,
          ;; and either find an acceptable window to split, or use the current
          ;; window.
          (edict-note-windows)
          (let ((use-window (edict-find-acceptable-window text-window)))
            (if use-window
                (setq edict-window use-window
                      text-window (split-window)) ; text-window height))
                (setq edict-window text-window))))
        ;; We have a window already.  Just adjust its size appropriately.
        ;; (unless (equal height (window-height edict-window))
        (let ((selected (selected-window)))
          (select-window edict-window)
          ;; (enlarge-window (- height (window-height edict-window)))
          (select-window selected)))
    ;; )
    (set-buffer edict-match-buffer)
    (let ((min (point-min))
          (inhibit-read-only t))
      ;; Replace everything.
      (erase-buffer)
      (mapcar (function (lambda (string-item)
                (insert string-item)
                (newline)))
              match-list)
      (when (eq *edict-window-location* 'bottom)
        (let ((w text-window))
          (setq text-window edict-window
                edict-window w)))
      ;; OK, now let's move the exact matches to the top.
      (goto-char min)
      ;; Be careful to preserve the order.
      ;; An exact match is any of "^key ", "[key]", "/key/", or "/to key/".
      (dolist (key (reverse key-list))
        (let* ((pattern (concat "^" key " \\|\\[" key "\\]\\|\\/" key
                                "\\/\\|\\/to " key "\\/" ))
               (top-lines nil))
          ;; First pull them out of the buffer into a list (top-lines).
          ;; Then re-insert them at the top.
          (while (re-search-forward pattern nil t)
            (forward-line 0)
            (let ((p (point)))
              (forward-line 1)
              (push (buffer-substring p (point)) top-lines)
              (delete-region p (point))))
          (goto-char min)
          (mapcar 'insert top-lines)))
      ;; OK, display it all.
      (set-window-buffer edict-window edict-match-buffer)
      (set-window-start edict-window min)
      (select-window edict-window)
      (when transpose-window-p
        (his-transpose-windows 1))))
  t)

(defun tsp-edict-search-kanji (word)
  "Search the word at point when given.
It presents the word at point as default input and allows editing
it."
  (interactive
   (list (read-string "Search kanji: " (current-word))))
  (unless word
    (setq word (read-string "Search kanji: ")))
  (edict-init)
  (edict-search-and-display (edict-clean-up-kanji word)
                            '日本語))

;; (global-set-key (kbd "M-S") 'tsp-edict-search-kanji)

;; search japanese on goo.ne.jp
(defun tsp-urllib-quote-plus (str from to)
  "Run python's urllib.quote_plus function.
FROM and TO are coding system symbols.
i.e.,
    urllib.quote_plus(STR.decode(FROM).encode(TO))"
  (let ((ret (shell-command-to-string
              (format "python -c \"import urllib;print urllib.quote_plus('%s'.decode('%s').encode('%s'))\"\n"
                      str
                      (symbol-name from)
                      (symbol-name to)))))
    (substring ret 0 (1- (length ret)))))

(defun tsp-search-jp (beg end)
  "Search WORD(utf8 coding) on goo.ne.jp in firefox."
  (interactive "r")
  (tsp-shell-command-asynchronously
   (concat
    "firefox -new-tab '"
    "http://dictionary.goo.ne.jp/search.php?MT="
    (tsp-urllib-quote-plus (buffer-substring beg end) 'utf-8 'euc-jp)
    "&search_history=%CA%D8%CD%F8&kind=all&kwassist=0&all.x=31&all.y=8&all=%BC%AD%BD%F1%A4%B9%A4%D9%A4%C6&mode=0"
    "'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (setq dictem-server "localhost")
;; (require 'dictem)
;; (dictem-initialize)
;; (define-key mode-specific-map [?s] 'dictem-run-search)


;; (define-key dictem-mode-map [tab] 'dictem-next-link)
;; (define-key dictem-mode-map [(backtab)] 'dictem-previous-link)

;;                                         ; For creating hyperlinks on database names
;;                                         ; and found matches.
;;                                         ; Click on them with mouse-2
;; (add-hook 'dictem-postprocess-match-hook
;;           'dictem-postprocess-match)

;;                                         ; For highlighting the separator between the definitions found.
;;                                         ; This also creates hyperlink on database names.
;; (add-hook 'dictem-postprocess-definition-hook
;;           'dictem-postprocess-definition-separator)

;;                                         ; For creating hyperlinks in dictem buffer
;;                                         ; that contains definitions.
;; (add-hook 'dictem-postprocess-definition-hook
;;           'dictem-postprocess-definition-hyperlinks)

;;                                         ; For creating hyperlinks in dictem buffer
;;                                         ; that contains information about a database.
;; (add-hook 'dictem-postprocess-show-info-hook
;;           'dictem-postprocess-definition-hyperlinks)

(provide '50dictionary)
