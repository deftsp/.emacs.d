;;;  Calendar
;; How about work with Google Calendars http://bc.tech.coop/blog/070306.html
(require 'cal-china-x nil t)

;;; diary
(setq diary-list-include-blanks nil
      calendar-view-diary-initially-flag nil
      ;; number-of-diary-entries '[7 7 7 7 7 9 8]
      diary-file (expand-file-name "~/.emacs.d/diary")
      diary-mail-addr "deftsp@gmail.com"
      calendar-mark-diary-entries-flag t)

(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;; (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;;; Appointments
(setq appt-display-diary t
      appt-display-duration 10
      appt-display-format 'window  ; use a separate window to remind appointments
      appt-message-warning-time 15 ; warn 15 min in advance
      appt-display-interval 5
      appt-audible t ; beep to indicate appointment
      appt-display-mode-line t)
(appt-activate 1)

;; appointments notification
;; http://article.gmane.org/gmane.emacs.orgmode/66151
(defvar pl/terminal-notifier-bin "terminal-notifier")

(defun pl/terminal-notification (title msg)
  (if (executable-find pl/terminal-notifier-bin)
      (shell-command (concat pl/terminal-notifier-bin " -message " msg " -title " title))
    (message (format "unable to find: %s" pl/terminal-notifier-bin))))

;; designate the window function for pl/appt-send-notification
(defun pl/appt-display (min-to-app new-time msg)
  (pl/terminal-notification
    (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
    (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call

(if (eq system-type 'darwin)
    (setq appt-disp-window-function #'pl/appt-display)
  (setq appt-disp-window-function #'appt-disp-window))

;; use grow to notification
;; (defun pl/grow-appt-display (min-to-app new-time msg)
;;   (growl (format "Appointment in %s minute(s)" min-to-app) msg t))


(setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
      calendar-date-display-form '(year "/" month "/" day)
      calendar-time-display-form
      '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

;;当你创建了一个'~/diary'文件，你就可以使用calendar去查看里面的内容。你可以查看当天的事件，相关命令如下 ：
;;  d     显示被选中的日期的所有事件
;;  s     显示所有事件，包括过期的，未到期的等等

;; 创建一个事件的样例：
;; 02/11/1989
;;     Bill B. visits Princeton today
;;     2pm Cognitive Studies Committee meeting
;;     2:30-5:30 Liz at Lawrenceville
;;     4:00pm Dentist appt
;;     7:30pm Dinner at George's
;;     8:00-10:00pm concert

;; 创建事件的命令：
;; i d   为当天日期添加一个事件
;; i w   为当天周创建一个周事件
;; i m   为当前月创建一个月事件
;; i y   为当前年创建一个年事件
;; i a   为当前日期创建一个周年纪念日
;; i c   创建一个循环的事件

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calendar-mark-holidays-flag t
      calendar-view-holidays-initially-flag nil
      calendar-week-start-day 1 ; a week in the calendar begins from Monday
      calendar-remove-frame-by-deleting t)

(setq calendar-location-name "Longshizhen"
      calendar-latitude +29.2 ;设置所在地的经纬度和地名，calendar 可以根据这些信息告知你每天的日出和日落的时间,日月食的预测
      calendar-longitude +105.1)

;; cancel these holidays
(setq holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-solar-holidays nil
      holiday-bahai-holidays nil)

;;----------------------------------------------------------------------------------------------------
;;; lunar
;;----------------------------------------------------------------------------------------------------
;; (setq calendar-chinese-celestial-stem
;;       ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
;; (setq  calendar-chinese-terrestrial-branch
;;        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
;; (setq chinese-calendar-month-name
;;       ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月"
;;               "十一月" "腊月"])

(setq holiday-other-holidays
      '((holiday-fixed 1 1 "元旦")
        (holiday-fixed 3  8  "妇女节")
        (holiday-fixed 3  12 "植树节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5  4  "青年节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-fixed 6  1  "儿童节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-fixed 9  10 "教师节")
        (holiday-fixed 10 1 "国庆节")
        (holiday-fixed 12 25 "圣诞节")
        (holiday-solar-term "清明" "清明节")

        (holiday-lunar 1 15 "元宵节")
        (holiday-lunar 5 5 "端午节" 0)
        (holiday-lunar 8 15 "中秋节" 0)
        (holiday-lunar 7 7  "七夕节")
        (holiday-lunar 9 9  "重阳节")
        (holiday-lunar 12 30 "春节" 0)))

(setq calendar-holidays holiday-other-holidays)




;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-anniversary-lunar (month-lunar day-lunar &optional year-lunar mark)
  (let* ((current-chinese-date (calendar-chinese-from-absolute
                                (calendar-absolute-from-gregorian
                                 date)))
         (ncycle (car current-chinese-date))
         (nyear  (cadr current-chinese-date))
         (ndate-chinese (list ncycle nyear month-lunar day-lunar))
         (ddate (calendar-gregorian-from-absolute
                 (calendar-chinese-to-absolute
                  ndate-chinese)))

         (cycle (if year-lunar
                    (+ (floor year-lunar 60) 45)
                    ncycle))
         (year (if year-lunar
                   (mod (- year-lunar 3) 60)
                   nyear))
         (date-chinese (list cycle year month-lunar day-lunar))
         (gregorian-of-date-chinese (calendar-gregorian-from-absolute
                                     (calendar-chinese-to-absolute date-chinese)))

         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (diff (if year-lunar (- yy
                                 (calendar-extract-year gregorian-of-date-chinese)) 100)))

    (and (= mm 2) (= dd 29) (not (calendar-leap-year-p yy))
       (setq mm 3 dd 1))
    (and (> diff 0) (equal  (list month-lunar day-lunar) (list (nth 2 current-chinese-date)
                                                             (nth 3 current-chinese-date)))
       (cons mark (format entry diff (diary-ordinal-suffix diff))))))



;; Calendar 模式支持各种方式来更改当前日期
;;（这里的"前"是指还没有到来的那一天，"后"是指已经过去的日子）
;;  q      退出calendar模式
;; C-f     让当前日期向前一天
;; C-b     让当前日期向后一天
;; C-n     让当前日期向前一周
;; C-p     让当前日期向后一周
;; M-}     让当前日期向前一个月
;; M-{     让当前日期向后一个月
;; C-x ]   让当前日期向前一年
;; C-x [   让当前日期向后一年
;; C-a     移动到当前周的第一天
;; C-e     移动到当前周的最后一天
;; M-a     移动到当前月的第一天
;; M-e     多动到当前月的最后一天
;; M-<     移动到当前年的第一天
;; M->     移动到当前年的最后一天

;;Calendar模式支持移动多种移动到特珠日期的方式
;; g d     移动到一个特别的日期
;;  o      使某个特殊的月分作为中间的月分
;;  .      移动到当天的日期
;; p d     显示某一天在一年中的位置，也显示本年度还有多少天。
;; C-c C-l 刷新Calendar窗口

;; Calendar支持生成LATEX代码。
;; t m     按月生成日历
;; t M     按月生成一个美化的日历
;; t d     按当天日期生成一个当天日历
;; t w 1   在一页上生成这个周的日历
;; t w 2   在两页上生成这个周的日历
;; t w 3   生成一个ISO-SYTLE风格的当前周日历
;; t w 4   生成一个从周一开始的当前周日历
;; t y     生成当前年的日历

;;EMACS Calendar支持配置节日：
;; h       显示当前的节日
;; x       定义当天为某个节日
;; u       取消当天已被定义的节日
;; e       显示所有这前后共三个月的节日。
;; M-x holiday  在另外的窗口的显示这前后三个月的节日。


;; 另外，还有一些特殊的，有意思的命令：
;; S       显示当天的日出日落时间(是大写的S)
;; p C     显示农历可以使用
;; g C     使用农历移动日期可以使用

(provide '50calendar)
