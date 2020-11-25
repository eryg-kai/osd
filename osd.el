;;; osd.el --- Emacs notification daemon. -*- lexical-binding: t -*-

;; Copyright (c) 2020 0x0049

;; Author: 0x0049 <dev@0x0049.me>
;; URL: https://github.com/0x0049/osd
;; Keywords: notifications dbus
;; Version: 1.0.1

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

(defcustom osd-max-notifications 1000
  "Maximum number of notifications to keep in memory."
  :type 'integer
  :group 'osd)

(defcustom osd-display-method 'echo
  "How to display new notifications."
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Notification buffer" buffer)
          (const :tag "Echo area" echo))
  :group 'osd)

(cl-defstruct notification time summary body actions)

(defvar osd--notification-ring nil "Notification list.")

;; Each notification gets an incrementing ID. Servers should never return zero
;; for an ID.
(defvar osd--id 0 "Last notification ID.")

(defun osd--dbus-close-notification (id)
  "Handle the CloseNotification signal.

Close a notification identified by ID. If the notification no
longer exists, an empty D-BUS message is sent back.

The NotificationClosed signal is emitted by this method."
  (message "TODO: CloseNotification: %s" id))

(defun osd--dbus-get-capabilities ()
  "Handle the GetCapabilities signal."
  '((;; "action-icons"  ;; Icons for actions instead of text.
    "actions"          ;; Provide actions to the user.
    "body"             ;; Body text.
    "body-hyperlinks"  ;; Hyperlinks in the body.
    ;; "body-images"   ;; Images in the body.
    ;; "body-markup"   ;; Markup in the body.
    ;; "icon-multi"    ;; Render an animation (gets multiple frames).
    ;; "icon-static"   ;; Show one frame (mutually exclusive with icon-multi).
    "persistence"      ;; Notifications are retained until removed by user.
    ;; "sound"         ;; Must support "sound-file" and "suppress-sound" hints.
    )))

(defun osd--dbus-get-server-information ()
  "Handle the GetServerInformation signal."
  '("osd"    ;; Name of the server.
    "0x0049" ;; Vendor name.
    "1.1"    ;; Version of the server.
    "1.2"    ;; Version of the spec with which the server is compliant.
    ))

(defun osd--dbus-notify (_app-name replaces-id _app-icon summary body actions _hints _expire_timeout)
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
                    :actions actions
                    :body body
                    :summary summary
                    :time (format-time-string osd-time-format)))
    id))

(defun osd--center-truncate (item len)
  "Replace the center of ITEM with … to make it of length LEN (including …).

When the length is odd the right side will be one longer than the left."
  (let ((item (if (stringp item) item (format "%s" item))))
    (if (> (length item) len)
        (let* ((len (- len 1))
               (mid (/ len 2)))
          (concat (substring item 0 mid)
                  (apply #'propertize "…" (text-properties-at (- mid 1) item))
                  (substring item (- mid len) nil)))
      item)))

(defun osd--entries ()
  "Return notification data for `tabulated-list-entries'."
  (let ((vect nil)
        (idx (- (or (and osd--notification-ring (ring-length osd--notification-ring)) 0) 1)))
    (while (and (>= idx 0))
      (let* ((entry (ring-ref osd--notification-ring idx))
             (notification (cdr entry)))
        (push
         `(,(car entry) [,(cl-struct-slot-value 'notification 'time notification)
                         ,(osd--center-truncate
                           (cl-struct-slot-value 'notification 'summary notification)
                           50)
                         ;; TODO: This still isn't great, would prefer to have
                         ;; the original newlines in addition to automatic
                         ;; wrapping but have it all aligned somehow.
                         ,(replace-regexp-in-string
                          "\n+" " "
                          (cl-struct-slot-value 'notification 'body notification))])
         vect))
      (setq idx (- idx 1)))
    vect))

(defun osd--refresh ()
  "Refresh the notification list."
  (setq tabulated-list-entries (osd--entries)))

(defun osd--goto-notification (id)
  "Goto to the notification identified by ID, staying on the same column.

If ID is not found, go to the beginning of the buffer."
  (unless (derived-mode-p 'osd-mode)
    (error "The current buffer is not in OSD mode"))
  (let ((col (tablist-current-column)))
    (goto-char (point-min))
    (while (and (not (equal id (tabulated-list-get-id)))
                (not (eq 1 (forward-line 1)))))
    (unless (tabulated-list-get-id) (goto-char (point-min)))
    (tablist-move-to-column
     (or col (car (tablist-major-columns))))))

(defun osd--get-notification (id)
  "Get a notification by ID."
  (let ((idx (- (or (and osd--notification-ring (ring-length osd--notification-ring)) 0) 1)))
    (while (and (>= idx 0)
                (not (eq id (car (ring-ref osd--notification-ring idx)))))
      (setq idx (- idx 1)))
    (when (>= idx 0) (ring-ref osd--notification-ring idx))))

(defconst osd--reason-expired   1 "Notification expired.")
(defconst osd--reason-dismissed 2 "Closed by user.")
(defconst osd--reason-closed    3 "Closed by CloseNotification.")
(defconst osd--reason-undefined 4 "Undefined/reserved reason.")

(defun osd--find-notification (id)
  "Return the index of the notification identified by ID."
  (let ((idx (- (or (and osd--notification-ring (ring-length osd--notification-ring)) 0) 1)))
    (while (and (>= idx 0)
                (not (eq id (car (ring-ref osd--notification-ring idx)))))
      (setq idx (- idx 1)))
    (when (>= idx 0) idx)))

(defun osd--delete-notification (id)
  "Delete a notification by ID."
  (when-let ((idx (osd--find-notification id)))
    (ring-remove osd--notification-ring idx)
    (osd--apply-dbus-fn
     #'dbus-send-signal
     `(("NotificationClosed" ,id ,osd--reason-dismissed)))))

(defun osd--visit-notification (id)
  "Trigger a notification's action by ID."
  (when-let ((idx (osd--find-notification id))
             (entry (cdr (ring-ref osd--notification-ring idx)))
             (actions (cl-struct-slot-value 'notification 'actions entry)))
    (osd--apply-dbus-fn
     #'dbus-send-signal
     `(("ActionInvoked" ,id ,(car actions))))))

;;;###autoload
(defun osd-notify (id notification)
  "Store NOTIFICATION by ID then refresh notification list."
  (if osd--notification-ring
      (unless (eq osd-max-notifications (ring-size osd--notification-ring))
        (ring-resize osd--notification-ring osd-max-notifications))
    (setq osd--notification-ring (make-ring osd-max-notifications)))
  (let ((existing (osd--get-notification id)))
    (if existing (setcdr existing notification)
      (ring-insert osd--notification-ring `(,id . ,notification))))
  (let ((buffer (get-buffer-create "*Notifications*")))
    (with-current-buffer buffer
      (osd-mode)
      (osd--refresh)
      (tablist-revert)
      (osd--goto-notification id))
    ;; TODO: Unread/unacknowledged count in modeline.
    (cl-case osd-display-method
      (echo (let ((body (replace-regexp-in-string "\n+" " " (cl-struct-slot-value 'notification 'body notification)))
                  (summary (cl-struct-slot-value 'notification 'summary notification)))
              (message "%s" (if (< 0 (length body)) (concat summary ": " body) summary))))
      (buffer (display-buffer buffer)))))

(defun osd--apply-dbus-fn (dbus-fn args)
  "Call DBUS-FN with ARGS which is a list of argument lists."
  (dolist (a args)
    (apply dbus-fn
           :session "org.freedesktop.Notifications"
           "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
           a)))

;;;###autoload
(defun osd-start ()
  "Start listening."
  (interactive)
  (osd--apply-dbus-fn
   #'dbus-register-method
   '(("CloseNotification"    osd--dbus-close-notification)
     ("GetCapabilities"      osd--dbus-get-capabilities)
     ("GetServerInformation" osd--dbus-get-server-information)
     ("Notify"               osd--dbus-notify))))

;;;###autoload
(defun osd-show-notifications ()
  "Show notifications buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*Notifications*"))
  (osd-mode)
  (osd--refresh)
  (tablist-revert))

;;;###autoload
(defun osd-stop ()
  "Stop listening."
  (interactive)
  (dbus-unregister-service :session "org.freedesktop.Notifications"))

(defun osd--tablist-operations (operation &rest arguments)
  "Perform OPERATION with ARGUMENTS.

See `tablist-operations-function' for more information."
  (cl-ecase operation
    (delete (mapc #'osd--delete-notification (nth 0 arguments)))
    (find-entry (osd--visit-notification (nth 0 arguments)))
    (supported-operations '(delete find-entry))))

(define-derived-mode osd-mode tablist-mode "OSD"
  "Mode for the notification center."
  (setq tabulated-list-format [("Time" 20 t)("Summary" 50 t)("Body" 50 t)]
        tabulated-list-padding 2
        tablist-operations-function 'osd--tablist-operations)
  (add-hook 'tabulated-list-revert-hook #'osd--refresh nil t)
  (tabulated-list-init-header)
  ;; TODO: Does inheriting tablist-mode not automatically set this?
  (tablist-minor-mode))

(defun osd--org-format-appt (remaining text)
  "Format appointment described by TEXT due in REMAINING minutes.

The result is a list with the summary and body."
  (let ((case-fold-search nil)
        (remaining (if (string= "0" remaining)
                       "now"
                     (concat "in " remaining " min"
                             (unless (string= "1" remaining) "s")))))
    (save-match-data
      ;; [START] [STATE? (3+ capitalized chars)] [SUMMARY] [START?] - [END?]
      ;; 10:00   TASK                            TEXT      10:00    - 11:00
      (if (string-match "^\\([0-9]+:[0-9]+\\) \\(?:\\([A-Z]\\{3,\\}\\) \\)?\\(.+?\\)\\(?: ?\\([0-9]+:[0-9]+\\)\\)?\\(?:-\\([0-9]+:[0-9]+\\)\\)?$" text)
          (let ((state (match-string 2 text))
                (summary (match-string 3 text))
                (start (or (match-string 4 text) (match-string 1 text)))
                (end (match-string 5 text)))
            `(,(concat summary " " remaining)
              ,(concat (when state (concat (capitalize state) " "))
                       "@ "
                       start (when end " to ") end
                       ".")))
        `(,(format "%s %s" text remaining) "")))))

(defun osd--org-single-appt-display (remaining text)
  "Display appointment described by TEXT due in REMAINING minutes."
  (apply 'call-process "notify-send" nil 0 nil (osd--org-format-appt remaining text)))

(defun osd--org-agenda-format-item (fn &rest args)
  "Append time to txt of the string returned by calling FN with ARGS.

The resulting string is returned.

This works in conjunction with `osd-org-appt-display' so it can
display the end time."
  (let* ((string (apply fn args))
         (time (org-find-text-property-in-string 'time string))
         (txt (org-find-text-property-in-string 'txt string)))
    (if (not (string-empty-p time))
        (org-add-props string nil 'txt (concat (org-trim txt) " " time))
      string)))

(advice-add 'org-agenda-format-item :around #'osd--org-agenda-format-item)

;;;###autoload
(defun osd-org-appt-display (remaining _current text)
  "Display appointment described by TEXT due in REMAINING (a string) minutes.

CURRENT is a string giving the current date.

The arguments may also be lists, where each element is a separate
appointment."
  (if (listp remaining)
      (dotimes (i (length remaining))
        (osd--org-single-appt-display (nth i remaining) (nth i text)))
    (osd--org-single-appt-display remaining text)))

(provide 'osd)

;;; osd.el ends here
