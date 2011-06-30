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

(defpclass ticket-box ()
  ((value
    :initform 0
    :accessor ticket-box-value)))

(defvar *ticket-box*
  (let ((tn (get-instances-by-class 'ticket-box)))
    (assert (endp (cdr tn)))
    (if tn
        (car tn)
        (create-instance 'ticket-box))))

;;; Persistent variable whose value is the count of tickets in the
;;; database. This is used to set new and unique ID to the tickets.
(define-symbol-macro *ticket-count*
    (ticket-box-value *ticket-box*))

(defpclass ticket ()
  (;; Indexed slots
   (id
    :type integer
    :initform (incf *ticket-count*)
    :reader ticket-id
    :index t)
   (status
    :initarg :status
    :type string
    :accessor ticket-status
    :index t)
   (created-by
    :initarg :created-by
    :type string
    :reader ticket-created-by
    :index t)
   ;; Non-indexed slots
   (description
    :initarg :description
    :type string
    :accessor ticket-description)
   (context
    :initarg :context
    :type string
    :reader ticket-context)
   (assign
    :initarg :assign
    :type (or string null)
    :accessor ticket-assign)
   ;; Timestamp at creation.
   (created
    :initarg :created
    :type integer
    :reader ticket-created)
   ;; This slot keeps a pset with the logs related to this ticket.
   (logs
    :initform (make-pset)
    :reader %ticket-logs)))

(defun query-ticket (id)
  (and (get-instance-by-value 'ticket 'id id)
       (%error "Unknown ticket identifier #~a." id)))

(defpclass ticket-log ()
  ((user
    :initarg :user
    :type user
    :reader ticket-log-user)
   (timestamp
    :initarg :timestamp
    :type integer
    :initform (get-universal-time)
    :reader ticket-log-timestamp)
   (action
    :initarg :action
    :type string
    :initform (required-arg)
    :reader ticket-log-action)))

(defun add-log (ticket user action)
  (insert-item (create-instance 'ticket-log :user user :action action)
               (%ticket-logs ticket)))

(defmacro do-ticket-logs ((var ticket) &body body)
  `(map-pset (lambda (,var) ,@body)
             (%ticket-logs ,ticket)))


;;; This variable keeps a btree which maps single WORDS to tickets
;;; whose description contains that word. Unsurprisingly it is used to
;;; search across the tickets.
(defvar *word-index*
  (or (get-from-root "word-index")
      (progn
        (let ((btree (make-btree)))
          (add-to-root "word-index" btree)
          btree))))

(defun add-word-to-index (word ticket)
  (let ((word (string-upcase word)))
    (let ((pset (get-value word *word-index*)))
      (if pset
          (insert-item ticket pset)
          (setf (get-value word *word-index*)
                (make-pset :items (list ticket)))))))

(defun find-tickets-with-word (word)
  (let ((pset (get-value word *word-index*)))
    (if pset
        (pset-list pset)
        nil)))

(defun search-ticket (list-of-words)
  (reduce #'intersection
          (loop for word in list-of-words
                append (find-tickets-with-word word))))


;;; Split a description in words.
(defun get-description-words (description)
  (mapcar #'string-upcase (split-string description " -+=,.!?()<>[]{}#$%&")))


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
           (let ((,ticket (query-ticket ,n)))
             (handler-case
                 (prog1 (progn ,code)
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
  (create-instance 'ticket :description descr :created-by *context-from* :contxt *context-to*)
  (response "Ticket #~a added for ~a." *ticket-count* *context-to*))

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
    (response "status: ~(~a~) ~@[by ~a~]"
              (ticket-status ticket)
              (ticket-assign ticket))))

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
    (let ((ticket-list
           ;; TODO: It hardly could be worse. Please, fix me!
           (sort (intersection
                  (get-instance-by-value 'ticket 'status status)
                  (union (get-instance-by-value 'ticket 'created-by user)
                         (get-instance-by-value 'ticket 'assign user)))
                 #'< :key #'ticket-created)))
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
  (let ((result (search-ticket words)))
    (if (null result)
        (response "No tickets.")
        (dolist (ticket result)
          (format-ticket ticket)))))

;;; taskbot-tracker.lisp ends here
