;;                                                               -*- Lisp -*-
;; taskbot-notification.lisp -- 
;;
;; Copyright (C) 2011 David Vazquez
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
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 

(in-package :taskbot)

(define-table notifications
  id "INTEGER PRIMARY KEY ASC"
  timestamp "INTEGER"
  context "TEXT"
  description "TEXT"
  sentp "BOOLEAN")

(defclass notification ()
  ((id
    :initarg :id
    :reader notification-id)
   (timestamp
    :initarg :timestamp
    :reader notification-timestamp)
   (context
    :initarg :context
    :reader notification-context)
   (description
    :initarg :description
    :reader notification-description)
   (sentp
    :initarg :sentp
    :reader notification-sent-p)))

(defun %list-to-notification (list)
  (destructuring-bind (id timestamp context description sentp)
      list
    (make-instance 'notification
                   :id id
                   :timestamp timestamp
                   :context context
                   :description description
                   :sentp (not (zerop sentp)))))

(defun create-notification (context description)
  (sqlite:execute-non-query *database* "
INSERT INTO notifications(timestamp, context, description, sentp)
VALUES (?, ?, ?, ?)" (get-universal-time) context description 0)
  (let ((id (sqlite:last-insert-rowid *database*)))
    (query-notification id)))

(defun query-notification (id)
  (%list-to-notification
   (multiple-value-list
    (sqlite:execute-one-row-m-v *database* "
SELECT id,timestamp,context,description,sentp 
FROM notifications WHERE id=?" id))))

(defun list-notifications (context &optional (only-pending t))
  (let ((result (sqlite:execute-to-list *database* "
SELECT id,timestamp,context,description,sentp
FROM notifications WHERE context=? AND sentp=?" context (if only-pending 0 1))))
    (mapcar #'%list-to-notification result)))

(defun clear-notification (notification)
  (setf (slot-value notification 'sentp) t)
  (sqlite:execute-non-query *database* "
UPDATE notifications SET sentp=1 WHERE id=?" (notification-id notification)))

(defun join-handler (message)
  (let* ((context (irc:source message))
         (notifications (list-notifications context)))
    (when notifications
      (immediate-response-to context "You have ~a~ news:" (length notifications)))
    (dolist (x notifications)
      (immediate-response-to context "~70a ~a ago"
                             (notification-description x)
                             (format-time (- (get-universal-time) (notification-timestamp x))
                                          :precission 2
                                          :abbrev t))
      (clear-notification x))))

(defun notificate-to (context fmt &rest args)
  (funcall #'create-notification context (apply #'format nil fmt args)))

(defun notificate (fmt &rest args)
  (apply #'notificate-to *context-from* fmt args))


;; taskbot-notification.lisp ends here
