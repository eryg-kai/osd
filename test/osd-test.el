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

(ert-deftest osd-test-pascal-to-kebab ()
  "Test converting PascalCase to kebab-case."
  (should (string= "test" (osd--pascal-to-kebab "Test")))
  (should (string= "test-this-var" (osd--pascal-to-kebab "TestThisVar"))))

(ert-deftest osd-test-appt-format ()
  "Test appointment formatting."
  (should (equal '("Foo in 10 mins" "@ 10:00 to 11:00.") (osd--org-format-appt "10" "10:00 Foo 10:00-11:00")))
  (should (equal '("Bar now" "Task @ 11:00.") (osd--org-format-appt "0" "11:00 TASK Bar 11:00")))
  (should (equal '("Baz in 1 min" "Habit @ 12:31 to 12:32.") (osd--org-format-appt "1" "12:31 HABIT Baz 12:31-12:32")))
  (should (equal '("Baz in 1 min" "@ 12:31 to 12:32.") (osd--org-format-appt "1" "12:31 Baz 12:31-12:32")))
  (should (equal '("Qux mumble in 1 min" "@ 10:00.") (osd--org-format-appt "1" "10:00 Qux mumble")))
  (should (equal '("thud garply plugh in 2 mins" "@ 13:00.") (osd--org-format-appt "2" "13:00 thud garply plugh")))
  (should (equal '("thud garply plugh in 2 mins" "") (osd--org-format-appt "2" "thud garply plugh"))))

(provide 'osd-test)

;;; osd-test.el ends here
