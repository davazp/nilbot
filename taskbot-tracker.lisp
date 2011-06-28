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

(define-table word_index
  word "TEXT"
  id "INTEGER")

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

;;; Split a description in words.
(defun get-description-words (description)
  (mapcar #'string-upcase (split-string description " -+=,.!?()<>[]{}#$%&")))

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
INSERT INTO tickets(type, description, context, status, assign, created, created_by)
VALUES(?, ?, ?, ?, ?, ?, ?)" type description context
status assign created created-by)
      (let ((id (sqlite:last-insert-rowid *database*)))
        (setf (slot-value ticket 'id) id)
        ;; Store words in word_index
        (dolist (word (get-description-words description))
          (sqlite:execute-non-query *database* "
INSERT INTO word_index(word, id) VALUES(?, ?)" word id))))))


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

(defmacro with-ticket-edit ((var n) &body code)
  `(let ((,var (query-ticket ,n)))
     ;; We do NOT unwind-protect this, because we want to avoid
     ;; database changes on error.
     (prog1 (progn ,@code)
       (save-ticket ,var))))

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
  (unless (integerp id)
    (%error "An integer was expected as ticket id."))
  (or (%sql-simple-query "SELECT * FROM tickets WHERE id=?" id)
      (%error "ticket #~a not found." id)))

(defun list-last-tickets (context &key status user)
  (apply #'%sql-query
         (format nil
                 "SELECT * FROM tickets WHERE context=?
                  ~@[AND (~{~*status=?~^ OR ~}~])
                  ~@[AND (assign=? OR created_by=?)~]
                  ORDER BY created DESC"
                 (mklist status)
                 user)
         `(,context
           ,@(mklist status)
           ,@(if user
                 (list user user)
                 nil))))


;;; Helper functions

(defun ticket-todo-p (x)
  (string= (ticket-status x) "TODO"))

(defun ticket-started-p (x)
  (string= (ticket-status x) "STARTED"))

(defun ticket-canceled-p (x)
  (string= (ticket-status x) "CANCELED"))

(defun ticket-done-p (x)
  (string= (ticket-status x) "DONE"))

(defun ticket-open-p (x)
  (or (ticket-todo-p x)
      (ticket-started-p x)))

(defun ticket-closed-p (x)
  (or (ticket-canceled-p x)
      (ticket-done-p x)))



(defun format-ticket (ticket &key (show-status t))
  (response "#~d ~@[~a~@T~]~a~@[~70T[~a]~]~%"
            (ticket-id ticket)
            (and show-status (ticket-status ticket))
            (truncate-string (ticket-description ticket) 60 "...")
            (and (ticket-assign ticket)
                 (fill-string (truncate-string (ticket-assign ticket) 8 ".") 8))))

(defun format-ticket-numbers (list)
  (format nil "~{#~d~#[~; and ~;, ~]~}" (mapcar #'ticket-id list)))

;;; Run CODE for each TICKET bound to the ticket whose id is in the
;;; result of TICKET-LIST evaluation.  Finally, POSTCODE is run with
;;; SUCCESS-TICKETS bound to a list of tickets where no errors
;;; ocurred.  On CODE, calls to FAILURE are treated as calls to
;;; %ERROR, but the error is delayed until the end of the macro.
(defmacro do-tickets-numbers ((ticket ticket-list success-tickets) code &body postcode)
  (with-gensyms (n list errors collect-error success collect-success)
    `(with-collectors ((,success nil ,collect-success)
                       (,errors nil ,collect-error))
       (let ((,list ,ticket-list))
         (dolist (,n ,list)
           (with-ticket-edit (,ticket ,n)
             (handler-case
                 (progn
                   ,code
                   (,collect-success ,ticket))
               (taskbot-error (error)
                 (,collect-error error)))))
         ;;
         (let ((,success-tickets ,success))
           ,@postcode)
         ;;
         (when ,errors
           (response "There were errors. Type ,more for futher information.")
           (more)
           (dolist (error ,errors)
             (response "~?"
                       (simple-condition-format-control error)
                       (simple-condition-format-arguments error))))))))



;;;; Taskbot commands

(define-command add (&unparsed-argument descr)
    ((:documentation "Add a ticket."))
  (let ((ticket (create-ticket *context-to* descr :created-by *context-from*)))
    (response "Ticket #~a added for ~a." (ticket-id ticket) *context-to*)))

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


;;;; Marking

;;; Check if TICKET was created by NICKNAME.
(defun ownership (ticket nickname)
  (string= (ticket-created-by ticket) nickname))

;;; Notificate to the creator of a ticket a change in the status. USER
;;; is the nickname of the user who made the change. ACTION is the
;;; verb in simple past of the action. TICKET is the ticket which
;;; changed.
(defun notificate-change (user action ticket)
  (unless (ownership ticket user)
    (notificate-to (ticket-created-by ticket)
                   "The user ~a ~a the ticket #~a: ~a"
                   user
                   action
                   (ticket-id ticket)
                   (truncate-string (ticket-description ticket) 30 "..."))))


(define-command take (number1 &rest others-numbers)
    ((:documentation "Take tickets."))
  (do-tickets-numbers (ticket (cons number1 others-numbers) success)
      (progn
        (unless (ticket-todo-p ticket)
          (%error "You cannot take ticket #~d. It is not opened." (ticket-id ticket)))
        (setf (ticket-assign ticket) *context-from*)
        (setf (ticket-status ticket) "STARTED")
        (notificate-change *context-from* "took" ticket))
    (case (length success)
      (0 (response "No tickets taken."))
      (1 (response "Ticket ~a taken by ~a."
                   (format-ticket-numbers success)
                   *context-from*))
      (t (response "Tickets ~a taken by ~a."
                  (format-ticket-numbers success)
                  *context-from*)))))

(define-command giveup (number1 &rest other-numbers)
    ((:documentation "Give up tickets."))
  (do-tickets-numbers (ticket (cons number1 other-numbers) success)
      (progn
        (unless (or (string= *context-permission* "admin")
                    (and (ticket-assign ticket)
                         (string= *context-from* (ticket-assign ticket))))
          (%error "Ticket #~d was already taken." (ticket-id ticket)))
        (when (ticket-todo-p ticket)
          (%error "Ticket #~d is opened." (ticket-id ticket)))
        (setf (ticket-assign ticket) nil)
        (setf (ticket-status ticket) "TODO")
        (notificate-change *context-from* "gave up" ticket))
    (case (length success)
      (0 (response "No ticket was given up."))
      (1 (response "Ticket ~a given up."  (format-ticket-numbers success)))
      (t (response "Tickets ~a given up." (format-ticket-numbers success))))))

(define-command done (number1 &rest other-numbers)
    ((:documentation "Mark a ticket as finished."))
  (do-tickets-numbers (ticket (cons number1 other-numbers) success)
      (cond
        ((ticket-todo-p ticket)
         (setf (ticket-assign ticket) *context-from*)
         (setf (ticket-status ticket) "DONE")
         (notificate-change *context-from* "finished" ticket)
         (when (ownership ticket *context-from*)
          (notificate-to (ticket-created-by ticket) "The user ~a finished the ticket #~a: ~a"
                         *context-from*
                         (ticket-id ticket)
                         (truncate-string (ticket-description ticket) 30 "..."))))
        ((ticket-started-p ticket)
         (unless (or (string= (ticket-assign ticket) *context-from*)
                     (string= *context-permission* "admin"))
           (%error "You have not permissions to close ticket #~d." (ticket-id ticket)))
         (setf (ticket-status ticket) "DONE")
         (notificate-change *context-from* "finished" ticket))
        ((ticket-closed-p ticket)
         (%error "Ticked #~d is already closed." (ticket-id ticket)))
        (t
         (%error "You could not close the ticket #~d." (ticket-id ticket))))
    (case (length success)
      (0 (response "No ticket marked as done."))
      (1 (response "Ticket ~a is done." (format-ticket-numbers success)))
      (t (response "Tickets ~a were done." (format-ticket-numbers success))))))

(define-command cancel (number1 &rest others-numbers)
    ((:aliases "DISCARD")
     (:documentation "Mark a ticket as canceled."))
  (do-tickets-numbers (ticket (cons number1 others-numbers) success)
      (cond
        ((ticket-todo-p ticket)
         (setf (ticket-assign ticket) *context-from*)
         (setf (ticket-status ticket) "CANCELED")
         (notificate-change *context-from* "canceled" ticket))
        ((ticket-started-p ticket)
         (unless (or (string= (ticket-assign ticket) *context-from*)
                     (string= *context-permission* "admin"))
           (%error "You cannot close ticket #~d." (ticket-id ticket)))
         (setf (ticket-status ticket) "CANCELED")
         (notificate-change *context-from* "canceled" ticket))
        ((ticket-closed-p ticket)
         (%error "Ticket #~d is already closed." (ticket-id ticket)))
        (t
         (%error "You cannot close ticket #~d." (ticket-id ticket))))
    (case (length success)
      (0 (response "No ticket canceled."))
      (1 (response "Ticket ~a was canceled." (format-ticket-numbers success)))
      (t (response "Tickets ~a were canceled." (format-ticket-numbers success))))))


;;;; Listing

;;; Returned a list of values, which is accept by the
;;; `list-last-tickets' function.
(defun resolve-status (x)
  (flet ((some-of (&rest list)
           (find x list :test #'string-ci=)))
    (cond
      ((some-of "TODO")
       (list "TODO"))
      ((some-of "STARTED")
       (list "STARTED"))
      ((some-of "DONE")
       (list "DONE"))
      ((some-of "CANCELED" "DISCARDED")
       (list "CANCELED"))
      ((some-of "OPEN" "OPENED")
       (list "TODO" "STARTED"))
      ((some-of "CLOSE" "CLOSED")
       (list "DONE" "CANCELED"))
      ((some-of "ALL")
       (list "TODO" "STARTED" "DONE" "CANCELED"))
      (t
       (%error "Wrong status `~a'." x)))))


(defun %ago (utime)
  (format-time (- (get-universal-time) utime) :precission 1))

(defun list-tickets (&optional kind user)
  (let* ((status (resolve-status kind))
         (show-status (> (length status) 1)))
    (let ((ticket-list (list-last-tickets *context-to* :status status :user user)))
      (if (null ticket-list)
         (response "No tickets.")
         (dolist (ticket ticket-list)
           (format-ticket ticket :show-status show-status))))))

(define-command list (&optional (kind "TODO"))
    ((:documentation "Show the last tickets."))
  (list-tickets kind))

(define-command inbox (&optional (kind "OPEN") (user *context-from*))
    ((:documentation "List the tickets created or assignated to USER."))
  (list-tickets kind user))

(define-command search (&unparsed-argument words)
    ((:documentation "Search the tickets whose description contains a list of words."))
  (let* ((word-list (get-description-words words))
         (sentence (format nil "SELECT * FROM tickets
                                WHERE context=? AND (status=\"TODO\" or status=\"STARTED\") AND
                                ID IN (SELECT id FROM word_index WHERE ~{~*word=?~^ OR ~})" word-list))
         (result (apply #'%sql-query sentence *context-to* word-list)))
    (if (null result)
        (response "No tickets.")
        (dolist (ticket result)
          (format-ticket ticket)))))

;;; taskbot-tracker.lisp ends here
