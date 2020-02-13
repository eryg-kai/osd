;;; test-helper.el --- osd test helpers. -*- lexical-binding: t -*-

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

;; Test helpers for osd.

;;; Code:

(require 'f)

(add-to-list 'load-path (f-dirname (f-dirname (f-this-file))))

;; https://github.com/rejeep/ert-runner.el/issues/49
(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

;;; test-helper.el ends here
