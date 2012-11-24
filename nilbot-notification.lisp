;;                                                               -*- Lisp -*-
;; nilbot-notification.lisp --
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

(in-package :nilbot)

(defpclass notification ()
  ((context
    :initarg :context
    :initform (required-arg)
    :reader notification-context
    :index t)
   (timestamp
    :initarg :timestamp
    :initform (get-universal-time)
    :reader notification-timestamp)
   (description
    :initarg :description
    :initform (required-arg)
    :reader notification-description)
   (sentp
    :initform nil
    :reader notification-sent-p)))

(defun list-notifications (context &optional (only-pending t))
  (let ((notifications (get-instances-by-value 'notification 'context context)))
    (if only-pending
        (remove t notifications :key #'notification-sent-p)
        notifications)))

(defun clear-notification (notification)
  (setf (slot-value notification 'sentp) t))

(defun join-handler (message)
  (let* ((context (irc:source message))
         (notifications (list-notifications context))
         (*immediate-response-p* t))
    (when notifications
      (response-to context "You have ~a news:" (length notifications)))
    (dolist (x notifications)
      (response-to context "~70a ~a ago"
                   (notification-description x)
                   (format-time (- (get-universal-time) (notification-timestamp x))
                                :precission 2
                                :abbrev t))
      (clear-notification x))))

(defun notificate-to (context fmt &rest args)
  (create-instance 'notification
                   :context context
                   :description (apply #'format nil fmt args)))

(defun notificate (fmt &rest args)
  (apply #'notificate-to *context-from* fmt args))


;; nilbot-notification.lisp ends here
