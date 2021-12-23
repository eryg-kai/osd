;;; osd-test.el --- Tests for osd. -*- lexical-binding: t -*-

;; Copyright (c) 2020 0x0049
;;
;; Author: 0x0049 <dev@0x0049.me>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests osd.

;;; Code:

(require 'osd)

(ert-deftest osd-test-appt-format ()
  "Test appointment formatting."
  (should (equal '("Foo in 10 mins" "@ 10:00 to 11:00.") (osd--org-format-appt "10" "10:00 Foo 10:00-11:00")))
  (should (equal '("Bar now" "Task @ 11:00.") (osd--org-format-appt "0" "11:00 TASK Bar 11:00")))
  (should (equal '("Baz in 1 min" "Habit @ 12:31 to 12:32.") (osd--org-format-appt "1" "12:31 HABIT Baz 12:31-12:32")))
  (should (equal '("Baz in 1 min" "@ 12:31 to 12:32.") (osd--org-format-appt "1" "12:31 Baz 12:31-12:32")))
  (should (equal '("Qux mumble in 1 min" "@ 10:00.") (osd--org-format-appt "1" "10:00 Qux mumble")))
  (should (equal '("thud garply plugh in 2 mins" "@ 13:00.") (osd--org-format-appt "2" "13:00 thud garply plugh")))
  (should (equal '("thud garply plugh in 2 mins" "") (osd--org-format-appt "2" "thud garply plugh"))))

(ert-deftest osd-test-internal-notify ()
  "Test that notifications are added."
  (should-not osd--notification-ring)
  (should-not (osd--entries))
  (should-not (osd--get-notification 1))
  (should-not (osd--delete-notification 1))

  (dotimes (i 3)
    (let ((notification (make-notification
                         :time (format-time-string osd-time-format)
                         :summary (format "summary %s" i)
                         :body (format "body %s" i))))
      (osd--notify i notification)))

  (should (eq 3 (ring-length osd--notification-ring)))

  (should (eq 2 (car (ring-ref osd--notification-ring 0))))
  (should (eq 1 (car (ring-ref osd--notification-ring 1))))
  (should (eq 0 (car (ring-ref osd--notification-ring 2))))

  (should (string= "summary 2" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "summary 1" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 1)))))
  (should (string= "body 0" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 2)))))

  (dotimes (i 3)
    (let ((notification (make-notification
                         :time (format-time-string osd-time-format)
                         :summary (format "summary replaced %s" i)
                         :body (format "body replaced %s" i))))
      (osd--notify i notification)))

  (should (eq 3 (ring-length osd--notification-ring)))

  (should (eq 2 (car (ring-ref osd--notification-ring 0))))
  (should (eq 1 (car (ring-ref osd--notification-ring 1))))
  (should (eq 0 (car (ring-ref osd--notification-ring 2))))

  (should (string= "summary replaced 2" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "summary replaced 1" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 1)))))
  (should (string= "body replaced 0" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 2)))))

  (let ((entries (osd--entries)))
    (should (eq 3 (length entries)))
    (should (string= "summary replaced 2" (aref (cadr (nth 0 entries)) 1)))
    (should (string= "summary replaced 1" (aref (cadr (nth 1 entries)) 1)))
    (should (string= "body replaced 0" (aref (cadr (nth 2 entries)) 2))))

  (osd--delete-notification 1)
  (should (eq 2 (ring-length osd--notification-ring)))
  (should (eq 2 (car (ring-ref osd--notification-ring 0))))
  (should (eq 0 (car (ring-ref osd--notification-ring 1))))

  (osd--tablist-operations 'delete '(0 2))
  (should (eq 0 (ring-length osd--notification-ring))))

(ert-deftest osd-test-notify ()
  (osd-notify '("foo" "bar"))

  (should (string= "foo" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "bar" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 0))))))

(ert-deftest osd-test-org-appt ()
  (osd-org-appt-display "10" nil "foo")

  (should (string= "foo in 10 mins" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 0)))))

  (osd-org-appt-display '("5" "15") nil '("bar" "bazzle"))

  (should (string= "bazzle in 15 mins" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 0)))))
  (should (string= "bar in 5 mins" (cl-struct-slot-value 'notification 'summary (cdr (ring-ref osd--notification-ring 1)))))
  (should (string= "" (cl-struct-slot-value 'notification 'body (cdr (ring-ref osd--notification-ring 1))))))

(provide 'osd-test)

;;; osd-test.el ends here
