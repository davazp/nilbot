;;                                                               -*- Lisp -*-
;; taskbot-commands.lisp --
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

;;; User who invoked taskbot and the target of taskbot ouptut
;;; respectively. They are dynamically bound when a privmsg is
;;; received.
(defvar *context-from*)
(defvar *context-to*)

;;; non-NIL if the server supports the capability IDENTIFY-MSG. We use
;;; this in order to be confident of the user and does not require
;;; authentication.
(defvar *identify-msg-p* nil)

(defun response-to (to fmt &rest args)
  (irc:privmsg *irc* to (apply #'format nil fmt args)))

(defun response (fmt &rest args)
  (apply #'response-to *context-to* fmt args))

(defun me-p (str)
  (string= str (irc:nickname (irc:user *irc*))))

(defun cap-handler (message)
  (destructuring-bind (target &optional response capabilities)
      (irc:arguments message)
    (declare (ignore target))
    ;; The *IDENTIFY-MSG-P* variable is dangerous. We must be
    ;; conservative here and set *IDENTIFY-MSG-P* when we are sure the
    ;; server support the capability.
    (setf *identify-msg-p* nil)
    (when (and response (string= response "ACK"))
      (let ((capabs (split-string capabilities)))
        (when (find "identify-msg" capabs :test #'string=)
          (setf *identify-msg-p* t))))))

(defun privmsg-handler (message)
  (let ((source (irc:source message)))
    (destructuring-bind (target input)
        (irc:arguments message)
      (process-message source target input))))

(defun process-message (origin target message)
  ;; If the IDENTIFY-MSG is avalaible, we require the user is
  ;; identified in the services of the IRC server.
  (when *identify-msg-p*
    (if (char= (char message 0) #\+)
        (setq message (subseq message 1))
        (return-from process-message)))
  ;; Process the message
  (let (prefixp)
    ;; Check if the message is a taskbot command
    (cond
      ((char= (char message 0) *default-prefix*)
       (setq prefixp t))
      ((me-p target)
       (setq prefixp nil))
      (t
       (return-from process-message nil)))
    ;; Invoke the command
    (with-input-from-string (stream message :start (if prefixp 1 0))
      (let ((cmd (parse-command stream))
            (arg (or (read-line stream nil) ""))
            (to (if (me-p target) origin target)))
        (let ((*context-from* origin)
              (*context-to* to))
          (run-command cmd arg))))))



;;; This hashtable keeps the association between strings and handlers.
(defvar *command-handlers*
  (make-hash-table :test #'equal))

(defclass handler ()
  ((documentation
    :initarg :documentation
    :reader handler-documentation)
   (function
    :initarg :function
    :initform (required-arg)
    :reader handler-function)
   (parse-arguments-p
    :initarg :parse-arguments-p
    :initform t
    :reader handler-parse-arguments-p)
   (aliases
    :initarg :aliases
    :initform ()
    :reader handler-aliases)))

(defclass alias ()
  ((handler
    :initarg :handler
    :initform (required-arg)
    :reader alias-handler)))

(define-predicate-type handler)
(define-predicate-type alias)

(defun find-handler (handler-spec)
  (etypecase handler-spec
    (null
     nil)
    (handler
     handler-spec)
    (alias
     (alias-handler handler-spec))
    (string
     (let ((entry (gethash (string-upcase handler-spec) *command-handlers*)))
       (find-handler entry)))))


(defun create-command (name documentation function parse-arguments-p)
  (setf (gethash name *command-handlers*)
        (make-instance 'handler
                       :documentation documentation
                       :function function
                       :parse-arguments-p parse-arguments-p)))

(defun create-command-alias (alias command)
  (let ((handler (find-handler command)))
    (if handler
        (setf (gethash alias *command-handlers*)
              (make-instance 'alias :handler handler))
        (error "The command `~a' does not exist." command))))


(defun run-command (command argument-line)
  (let ((handler (find-handler command)))
    (cond
      ((null handler)
       ;; unknown command
       )
      ;; Commands with parsed arguments
      ((handler-parse-arguments-p handler)
       (with-input-from-string (stream argument-line)
         (apply (handler-function handler)
                (parse-arguments stream))))
      ;; Command with raw argument
      ((not (handler-parse-arguments-p handler))
       (funcall (handler-function handler) argument-line)))))


(defun arglist-unparsed-argument-p (arglist)
  (and (not (null arglist))
       (eq (car arglist) '&unparsed-argument)))

(defmacro define-command (name (&rest args) options &body code)
  (let ((documentation (cadr (assoc :documentation options)))
        (aliases (cdr (assoc :aliases options)))
        (fname (symbolize 'irc-handler- name)))
    (if (arglist-unparsed-argument-p args)
        `(progn
           (defun ,fname ,(cdr args) ,@code)
           (create-command ,(string name) ,documentation ',fname nil)
           (dolist (alias ',aliases)
             (create-command-alias alias ,(string name))))
        `(progn
           (defun ,fname ,args ,@code)
           (create-command ,(string name) ,documentation ',fname t)
           (dolist (alias ',aliases)
             (create-command-alias alias ,(string name)))))))


;; taskbot-commands.lisp ends here
