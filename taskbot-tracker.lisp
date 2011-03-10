;;                                                               -*- Lisp -*-
;; taskbot-system.lisp --
;;
;; Copyright (C) 2009,2011 David Vazquez
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

(define-table tickets
  id "INTEGER PRIMARY KEY ASC"
  type "TEXT"
  description "TEXT"
  context "TEXT"
  status "TEXT"
  assign "TEXT"
  created "INTEGER"
  created_by "TEXT")

(define-table tickets_logs
  ticket "INTEGER"
  date "INTEGER"
  user "TEXT"
  action "TEXT")

(defclass ticket ()
  ((id
    :initarg :id
    :type integer
    :reader ticket-id)
   (type
    :initarg :type
    :type string
    :reader ticket-type)
   (description
    :initarg :description
    :type string
    :reader ticket-description)
   (context
    :initarg :context
    :type string
    :reader ticket-context)
   (status
    :initarg :status
    :type string
    :accessor ticket-status)
   (assign
    :initarg :assign
    :type (or string null)
    :accessor ticket-assign)
   (created
    :initarg :created
    :type integer
    :reader ticket-created)
   (created-by
    :initarg :created-by
    :type string
    :reader ticket-created-by)))

(defun create-ticket (context description &key (type "TASK") (status "TODO")
                      assign (created (get-universal-time)) created-by)
  (let ((ticket
         (make-instance 'ticket
                        :type type
                        :description description
                        :context context
                        :status status
                        :assign assign
                        :created created
                        :created-by created-by)))
    (prog1 ticket
      (sqlite:execute-non-query *database* "
INSERT INTO tickets(
   type,
   description,
   context,
   status,
   assign,
   created,
   created_by)
VALUES(?, ?, ?, ?, ?, ?, ?)" type description context
status assign created created-by)
      (setf (slot-value ticket 'id) (sqlite:last-insert-rowid *database*)))))


(defun save-ticket (ticket)
  (if (slot-boundp ticket 'id)
      ;; If ID is bound, the ticket has already been stored, then we
      ;; update the database with the current slots values.
      (with-slots (id type description context status assign created created-by)
          ticket
        (sqlite:execute-non-query *database* "
UPDATE tickets SET
   type=?,
   description=?,
   context=?,
   status=?,
   assign=?,
   created=?,
   created_by=?
   WHERE id=?" type description context status
assign created created-by id))))


(defun %list-to-ticket (list)
  (destructuring-bind (id type description context status
                          assign created created-by)
      list
    (make-instance 'ticket
                   :id id                   :type type
                   :description description :context context
                   :status status           :assign assign
                   :created-by created-by   :created created)))


(defun %sql-query (sql &rest args)
  (mapcar #'%list-to-ticket (apply #'sqlite:execute-to-list *database* sql args)))

(defun %sql-simple-query (sql &rest args)
  (let ((result
         (multiple-value-list
          (apply #'sqlite:execute-one-row-m-v *database* sql args))))
    (and result (%list-to-ticket result))))

(defun query-ticket (id)
  (or (%sql-simple-query "SELECT * FROM tickets WHERE id=?" id)
      (%error "ticket #~a not found." id)))

(defun list-last-tickets (context &key (limit 5) status)
  (if status
      (%sql-query
       "SELECT * FROM tickets WHERE context=? and status=? ORDER BY created DESC LIMIT ?"
       context status limit)
      (%sql-query
       "SELECT * FROM tickets WHERE context=? ORDER BY created DESC LIMIT ?"
       context limit)))


;;;; Taskbot commands

(define-command add (&unparsed-argument descr)
    ((:documentation "Add a task."))
  (let ((task (create-ticket *context-to* descr :created-by *context-from*)))
    (response "task #~a added for ~a." (ticket-id task) *context-to*)))

(define-command take (n)
    ((:documentation "Take a task."))
  (let ((ticket (query-ticket n)))
    (unless (string= (ticket-status ticket) "TODO")
      (%error "This task is not opened."))
    (setf (ticket-assign ticket) *context-from*)
    (setf (ticket-status ticket) "STARTED")
    (save-ticket ticket)
    (response "Task #~d taken." n)))

(define-command done (n)
    ((:documentation "Mark a task as finished."))
  (let ((ticket (query-ticket n)))
    (cond
      ((string= (ticket-status ticket) "TODO")
       (setf (ticket-assign ticket) *context-from*)
       (setf (ticket-status ticket) "DONE")
       (save-ticket ticket)
       (response "Task #~d done." n))
      ((string= (ticket-status ticket) "STARTED")
       (unless (or (string= (ticket-assign ticket) *context-from*)
                   (string= *context-permission* "admin"))
         (%error "You are not permissions to do this."))
       (setf (ticket-status ticket) "DONE")
       (save-ticket ticket)
       (response "Task #~d done." n)))))


(defun %ago (utime)
  (format-time (- (get-universal-time) utime) :precission 1 :abbrev t))

(define-command list ()
    ((:documentation "Show the last tickets."))
  (let ((ticket-list (list-last-tickets *context-to* :limit 5 :status "TODO")))
    (if (null ticket-list)
        (response "No open tasks.")
        (dolist (ticket ticket-list)
          (response "#~d ~a" (ticket-id ticket) (ticket-description ticket))))))

(define-command info (id)
    ((:documentation "Show information about the specified ticket."))
  (let ((ticket (query-ticket id)))
    (when (and (char/= (char (ticket-context ticket) 0) #\#)
               (string/= (ticket-context ticket) *context-from*))
      (%error "This is a private ticket."))
    (response "#~d ~a" (ticket-id ticket) (ticket-description ticket))
    (if (char= (char (ticket-context ticket) 0) #\#)
        (response "created by: ~a ~a ago at ~a channel"
                  (ticket-created-by ticket)
                  (%ago (ticket-created ticket))
                  (ticket-context ticket))
        (response "created by: ~a ~a ago"
                  (ticket-created-by ticket)
                  (%ago (ticket-created ticket))))
    (response "status: ~(~a~) ~@[by ~a~]" (ticket-status ticket) (ticket-assign ticket))))


;;; taskbot-tracker.lisp ends here
