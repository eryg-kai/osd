;;; osd-test.el --- Tests for osd. -*- lexical-binding: t -*-

;; Copyright (c) 2020 0x0049
;;
;; Author: 0x0049 <dev@0x0049.me>
;; URL: https://github.com/0x0049/osd
;; Keywords: notifications dbus
;; Version: 1.0

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

;;; Code:

(require 'osd)

(ert-deftest osd-test-pascal-to-kebab ()
  (should (string= "test" (osd--pascal-to-kebab "Test")))
  (should (string= "test-this-var" (osd--pascal-to-kebab "TestThisVar"))))

(provide 'osd-test)

;;; osd-test.el ends here
