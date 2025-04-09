;;; 50logview.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: languages

(use-package logview
  :straight t
  :custom
  (logview-additional-submodes
   '(("TL-MAIN" . (
                   (format . "TIMESTAMP IGNORED LEVEL ")
                   ;; (format . "TIMESTAMP <<[0-9]+ms>>")
                   (levels . "SLF4J")
                   (timestamp . "LogCat")))))

  (logview-additional-timestamp-formats
   '(("LogCat" (java-pattern . "yyyy-MM-dd HH:mm:ss.SSSX"))))
  :config

  (general-define-key
   :states 'normal
   :keymaps 'logview-mode-map
   "j" 'logview-next-entry
   "k" 'logview-previous-entry
   ;;; copy from the logview.el

   "TAB" 'logview-go-to-message-beginning
   "n"   'logview-next-entry
   "p"   'logview-previous-entry
   "N"   'logview-next-as-important-entry
   "P"   'logview-previous-as-important-entry
   "M-n" 'logview-next-navigation-view-entry
   "M-p" 'logview-previous-navigation-view-entry
   "<"   'logview-first-entry
   ">"   'logview-last-entry
   ;; Narrowing/widening commands.
   "["   'logview-narrow-from-this-entry
   "]"   'logview-narrow-up-to-this-entry
   "w"   'logview-widen
   "{"   'logview-widen-upwards
   "}"   'logview-widen-downwards
   "y"   'logview-narrow-to-thread
   "Y"   'logview-edit-thread-narrowing-filters
   ;; Filtering by level commands.
   "l 1" 'logview-show-only-errors
   "l e" 'logview-show-only-errors
   "l 2" 'logview-show-errors-and-warnings
   "l w" 'logview-show-errors-and-warnings
   "l 3" 'logview-show-errors-warnings-and-information
   "l i" 'logview-show-errors-warnings-and-information
   "l 4" 'logview-show-errors-warnings-information-and-debug
   "l d" 'logview-show-errors-warnings-information-and-debug
   "l 5" 'logview-show-all-levels
   "l t" 'logview-show-all-levels
   "+"   'logview-show-only-as-important
   "l +" 'logview-show-only-as-important
   "L 1" 'logview-always-show-errors
   "L e" 'logview-always-show-errors
   "L 2" 'logview-always-show-errors-and-warnings
   "L w" 'logview-always-show-errors-and-warnings
   "L 3" 'logview-always-show-errors-warnings-and-information
   "L i" 'logview-always-show-errors-warnings-and-information
   "L 4" 'logview-always-show-errors-warnings-information-and-debug
   "L d" 'logview-always-show-errors-warnings-information-and-debug
   "L 0" 'logview-disable-unconditional-show
   "L L" 'logview-disable-unconditional-show
   ;; Filtering by name/thread/message commands.
   "f"   'logview-edit-filters
   "a"   'logview-add-include-name-filter
   "A"   'logview-add-exclude-name-filter
   "t"   'logview-add-include-thread-filter
   "T"   'logview-add-exclude-thread-filter
   "m"   'logview-add-include-message-filter
   "M"   'logview-add-exclude-message-filter
   ;; Filter resetting commands.
   "r l" 'logview-reset-level-filters
   "r a" 'logview-reset-name-filters
   "r t" 'logview-reset-thread-filters
   "r m" 'logview-reset-message-filters
   "R"   'logview-reset-all-filters
   "r e" 'logview-reset-all-filters-restrictions-and-hidings
   ;; View commands.
   "v"   'logview-switch-to-view
   "V c" 'logview-set-section-view
   "V n" 'logview-set-navigation-view
   "V h" 'logview-highlight-view-entries
   "V u" 'logview-unhighlight-view-entries
   "V s" 'logview-save-filters-as-view-for-submode
   "V S" 'logview-save-filters-as-global-view
   "V e" 'logview-edit-submode-views
   "V E" 'logview-edit-all-views
   "V i" 'logview-assign-quick-access-index
   "V d" 'logview-delete-view
   ;; Section commands.
   "c a" 'logview-go-to-section-beginning
   "c e" 'logview-go-to-section-end
   "c n" 'logview-next-section
   "c p" 'logview-previous-section
   "c N" 'logview-next-section-any-thread
   "c P" 'logview-previous-section-any-thread
   "c ," 'logview-first-section
   "c ." 'logview-last-section
   "c <" 'logview-first-section-any-thread
   "c >" 'logview-last-section-any-thread
   "c c" 'logview-narrow-to-section
   "c C" 'logview-narrow-to-section-keep-threads
   "c h" 'logview-toggle-narrow-to-section-headers
   "c t" 'logview-toggle-sections-thread-bound
   ;; Explicit entry hiding/showing commands.
   "h"   'logview-hide-entry
   "H"   'logview-hide-region-entries
   "s"   'logview-show-entries
   "S"   'logview-show-region-entries
   "r h" 'logview-reset-manual-entry-hiding
   ;; Showing/hiding entry details commands.
   "d"   'logview-toggle-entry-details
   "D"   'logview-toggle-region-entry-details
   "e"   'logview-toggle-details-globally
   "r d" 'logview-reset-manual-details-hiding
   ;; Entry timestamp commands.
   "z a" 'logview-difference-to-current-entry
   "z t" 'logview-thread-difference-to-current-entry
   "z c" 'logview-difference-to-section-headers
   "z z" 'logview-go-to-difference-base-entry
   "z A" 'logview-forget-difference-base-entries
   "z T" 'logview-forget-thread-difference-base-entry
   "z C" 'logview-cancel-difference-to-section-headers
   "z n" 'logview-next-timestamp-gap
   "z p" 'logview-previous-timestamp-gap
   "z N" 'logview-next-timestamp-gap-in-this-thread
   "z P" 'logview-previous-timestamp-gap-in-this-thread
   "z g" 'logview-change-target-gap-length
   ;; Option changing commands.
   "o r" 'auto-revert-mode
   "o t" 'auto-revert-tail-mode
   "o v" 'logview-toggle-copy-visible-text-only
   "o m" 'logview-toggle-search-only-in-messages
   "o p" 'logview-toggle-filter-preview
   "o e" 'logview-toggle-show-ellipses
   "o g" 'logview-change-target-gap-length
   "o s" 'logview-choose-submode
   "o S" 'logview-customize-submode-options
   ;; For compatibility with the inactive keymap.
   "C-c C-c" 'logview-choose-submode
   "C-c C-s" 'logview-customize-submode-options
   ;; Miscellaneous commands.
   "SPC" 'logview-pulse-current-entry
   "?"   'logview-mode-help
   "g"   'logview-refresh-buffer-as-needed
   "G"   'logview-prepare-for-new-contents
   "x"   'logview-append-log-file-tail
   "X"   'logview-revert-buffer
   "q"   'bury-buffer
   ;; Simplified universal argument command
   ;; rebindings.  Digits and minus are set up by
   ;; 'suppress-keymap' already.
   "u"   'universal-argument))

(provide '50logview)
