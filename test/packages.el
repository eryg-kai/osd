;;; packages.el --- Install packages for testing. -*- lexical-binding: t -*-

(require 'package)

;;; Code:

(defun osd-test-install ()
  "Install packages for testing."
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
  (package-install 'tablist))

(provide 'packages)

;;; packages.el ends here
