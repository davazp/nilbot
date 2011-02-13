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

;;; User who invoked taskbot, target of taskbot ouptut and the user
;;; permissions respectively. They are dynamically bound when a
;;; privmsg is received.
(defvar *context-from*)
(defvar *context-to*)
(defvar *context-permission*)

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
  (with-simple-restart (irc-toplevel "Return to IRC toplevel loop.")
    (let ((source (irc:source message)))
      (destructuring-bind (target input)
          (irc:arguments message)
        (process-message source target input)))))


;;; IRC Errors. Taskbot captures these errors and report in IRC chat.
(define-condition taskbot-error (simple-error)
  nil)

;;; Signal a taskbot error.
(defun %error (fmt &rest args)
  (signal 'taskbot-error :format-control fmt :format-arguments args))


(defun process-message (origin target message)
  ;; If the IDENTIFY-MSG is avalaible, we require the user is
  ;; identified in the services of the IRC server.
  (when *identify-msg-p*
    (if (char= (char message 0) #\+)
        (setq message (subseq message 1))
        (return-from process-message)))
  ;; Process the message
  (let (prefix)
    ;; Check if the message is a taskbot command
    (cond
      ((char= (char message 0) *default-prefix*)
       (setq prefix 1))
      ;; 'taskbot: ' messages
      ((let* ((mark (format nil "~a: " (irc:nickname (irc:user *irc*))))
              (posi (search mark message)))
         (and (integerp posi) (zerop posi)))
       (setq prefix (length (format nil "~a: " (irc:nickname (irc:user *irc*))))))
      ;; queries
      ((me-p target)
       (setq prefix 0))
      (t
       (return-from process-message nil)))
    ;; Invoke the command
    (with-input-from-string (stream message :start prefix)
      (let ((cmd (parse-command stream))
            (arg (or (read-line stream nil) ""))
            (to (if (me-p target) origin target)))
        (let ((*context-from* origin)
              (*context-to* to)
              (*context-permission* (get-user-permission origin)))
          (handler-case
              (run-command cmd arg)
            (taskbot-error (error)
              (apply #'response
                     (simple-condition-format-control error)
                     (simple-condition-format-arguments error)))))))))

;;; Permissions functions

(deftype permission ()
  `(satisfies permissionp))

(defun permissionp (x)
  (find x #("nobody" "user" "admin") :test #'string-ci=))

(defun get-user-permission (user)
  (or (nth-value 2 (db-query-user user)) "nobody"))

(defun permission<= (perm1 perm2)
  (declare (permission perm1 perm2))
  (let ((perms #("nobody" "user" "admin")))
    (<= (position perm1 perms :test #'string-ci=)
        (position perm2 perms :test #'string-ci=))))

;;; Require PERM priviledge level.
(defun require-permission (perm)
  (unless (permission<= perm *context-permission*)
    (if (find (char perm 0) "aeiou")
        (%error "You need be an ~a to do this." perm)
        (%error "You need be a ~a to do this." perm))))



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
   ;; This slot is only informative. The function `run-command' will
   ;; work even if this slot is T and the user is not an admin. Each
   ;; command is responsible to require to the user the required
   ;; priviledges.
   (permission
    :initarg :permission
    :initform "user"
    :reader handler-permission)
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

(defun create-command (name documentation function parse-arguments-p permission)
  (setf (gethash name *command-handlers*)
        (make-instance 'handler
                       :documentation documentation
                       :function function
                       :parse-arguments-p parse-arguments-p
                       :permission permission)))

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
       (%error "unknown command"))
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

;;; Define a taskbot command. You can see taskbot-system.lisp for
;;; several examples of usage.
(defmacro define-command (name (&rest args) options &body code)
  (let ((documentation (cadr (assoc :documentation options)))
        (aliases (cdr (assoc :aliases options)))
        (permission (or (second (assoc :permission options)) "user"))
        (fname (symbolize 'irc-handler- name)))
    (check-type documentation (or null string))
    (check-type permission permission)
    (if (arglist-unparsed-argument-p args)
        `(progn
           (defun ,fname ,(cdr args)
             ,(if permission `(require-permission ,permission))
             ,@code)
           (create-command ,(string name) ,documentation ',fname nil ,permission)
           (dolist (alias ',aliases)
             (create-command-alias alias ,(string name))))
        `(progn
           (defun ,fname ,args
             ,(if permission `(require-permission ,permission))
             ,@code)
           (create-command ,(string name) ,documentation ',fname t ,permission)
           (dolist (alias ',aliases)
             (create-command-alias alias ,(string name)))))))


;;; Utility macro to define subcommands.
(defmacro subcommand-dispatch (subcommand arguments &body clausules)
  (check-type subcommand symbol)
  (with-gensyms (subcommand-var arguments-var)
    `(let ((,subcommand-var ,subcommand)
           (,arguments-var ,arguments))
       (cond
         ,@(loop for clausule in clausules
                 collect
                    (destructuring-bind ((name &rest args) &body code)
                        clausule
                      `((string-ci= ,subcommand-var ,name)
                        (destructuring-bind ,args ,arguments-var
                          ,@code))))
         (t
          (%error "~a is an invalid subcommand." ,subcommand-var))))))


;; taskbot-commands.lisp ends here
