;;; osd.el --- Emacs notification daemon. -*- lexical-binding: t -*-

;; Copyright (c) 2020 0x0049

;; Author: 0x0049 <dev@0x0049.me>
;; URL: https://github.com/0x0049/osd
;; Keywords: notifications dbus
;; Version: 1.0

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:

;; Emacs must be compiled with dbus support. Start Emacs with something like:
;; `exec dbus-launch --exit-with-session <emacs invocation>'.

;; See https://developer.gnome.org/notification-spec/ for the notification spec.

;;; Code:

(require 'dbus)
(require 'tablist)

(defcustom osd-time-format "%Y-%m-%d %T"
  "Format for the notification time column."
  :type 'string
  :group 'osd)

(defvar osd--dbus-signals
  '("ActionInvoked"
     "CloseNotification"
     "GetCapabilities"
     "GetServerInformation"
     "NotificationClosed"
     "Notify")
  "DBus signals.")

(defvar osd--listening nil "Whether currently listening for notifications.")

(cl-defstruct notification time summary body)

(defvar osd--notifications nil "Notification list.")

;; Each notification gets an incrementing ID. Servers should never return zero
;; for an ID.
(defvar osd--id 0 "Last notification ID.")

(defun osd--dbus-action-invoked (id action-key)
  "Handle the ActionInvoked signal.

Runs the action identified by ACTION-KEY (sent in the list of
actions with the notification) on a notification identified by
ID."
  (message "TODO: ActionInvoked: %s %s" id action-key))

(defun osd--dbus-close-notification (id)
  "Handle the CloseNotification signal.

Close a notification identified by ID. If the notification no
longer exists, an empty D-BUS message is sent back.

The NotificationClosed signal is emitted by this method."
  (message "TODO: CloseNotification: %s" id))

(defun osd--dbus-get-capabilities ()
  "Handle the GetCapabilities signal."
  '(;; "action-icons"  ;; Icons for actions instead of text.
    ;; "actions"       ;; Provide actions to the user.
    "body"             ;; Body text.
    "body-hyperlinks"  ;; Hyperlinks in the body.
    ;; "body-images"   ;; Images in the body.
    ;; "body-markup"   ;; Markup in the body.
    ;; "icon-multi"    ;; Render an animation (gets multiple frames).
    ;; "icon-static"   ;; Show one frame (mutually exclusive with icon-multi).
    "persistence"      ;; Notifications are retained until removed by user.
    ;; "sound"         ;; Must support "sound-file" and "suppress-sound" hints.
    ))

(defun osd--dbus-get-server-information ()
  "Handle the GetServerInformation signal."
  '("osd"     ;; Name of the server.
    "0x0049"  ;; Vendor name.
    "1.0"     ;; Version of the server.
    "1"     ;; Version of the spec with which the server is compliant.
    ))

(defun osd--dbus-notification-closed (id reason)
  "Handle the NotificationClosed signal.

Signal that a notification identiified by ID has been closed
because of REASON:
  1 - Expired.
  2 - Dismissed by the user.
  3 - Closed by a call to CloseNotification.
  4 - Undefined/reserved reasons."
  (message "TODO: NotificationClosed: %s %s" id reason))

(defun osd--dbus-notify (_app-name replaces-id _app-icon summary body _actions _hints _expire_timeout)
  "Handle the Notify signal.

APP-NAME is the optional name of the application sending the
notification.

REPLACES-ID is the optional ID of a notification this
notification replaces. If this is zero, the return value is an ID
that represents the notification. Otherwise it's the same as
REPLACES-ID.

APP-ICON is the optional program icon of the calling application.

SUMMARY is a brief description of the notification while BODY is
the optional detailed body text.

ACTIONS are list of pairs. The even elements are the identifier
and the odd elements are the strings to display to the user.

HINTS are optional hints that can provide extra information to
the server like a PID.

EXPIRE-TIMEOUT is how long to display the notification before
automatically closing it. If -1 it depends on the server. If 0 it
never expires."
  (let ((id (if (and replaces-id (not (eq replaces-id 0)))
                replaces-id
              (setq osd--id (+ 1 osd--id))
              osd--id)))
    (osd-notify id (make-notification
                    :time (format-time-string osd-time-format)
                    :summary summary
                    :body body))
    id))

(defun osd--pascal-to-kebab (var)
  "Convert VAR from PascalCase to kebab-case."
  (let ((case-fold-search nil))
    (downcase (replace-regexp-in-string "\\(.\\)\\([A-Z]+\\)" "\\1-\\2" var))))

(defun osd--entries ()
  "Return notification data for `tabulated-list-entries'."
  (mapcar
   #'(lambda (entry)
       (let ((notification (cdr entry)))
         `(,(car entry) [,(cl-struct-slot-value 'notification 'time notification)
                         ,(cl-struct-slot-value 'notification 'summary notification)
                         ,(cl-struct-slot-value 'notification 'body notification)])))
   osd--notifications))

(defun osd-refresh ()
  "Refresh the notification list."
  (setq tabulated-list-entries (osd--entries)))

;;;###autoload
(defun osd-notify (id notification)
  "Store NOTIFICATION by ID then refresh notification list."
  (let ((existing (assq id osd--notifications)))
    (if existing (setcdr existing notification)
      (push `(,id . ,notification) osd--notifications)))
  (let ((buffer (get-buffer-create "*Notifications*")))
    (with-current-buffer buffer
      (osd-mode)
      (osd-refresh)
      (tablist-revert))
    (pop-to-buffer buffer)))

;;;###autoload
(defun osd-start ()
  "Start listening."
  (interactive)
  (when osd--listening (user-error "Already listening"))
  (setq osd--listening t)
  (dolist (s osd--dbus-signals)
    (dbus-register-method
     :session "org.freedesktop.Notifications"
     "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
     s (intern (format "osd--dbus-%s" (osd--pascal-to-kebab s))))))

;;;###autoload
(defun osd-show-notifications ()
  "Show notifications buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*Notifications*")))

;;;###autoload
(defun osd-stop ()
  "Stop listening."
  (interactive)
  (unless osd--listening (user-error "Not listening"))
  (setq osd--listening nil)
  (dbus-unregister-service :session "org.freedesktop.Notifications"))

(define-derived-mode osd-mode tabulated-list-mode "OSD"
  "Mode for the notification center."
  (setq tabulated-list-format [("Time" 20 t)("Summary" 20 t)("Body" 30 t)]
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'osd-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'osd)

;;; osd.el ends here
