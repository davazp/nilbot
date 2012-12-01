;;                                                               -*- Lisp -*-
;; nilbot-commands.lisp --
;;
;; Copyright (C) 2009,2011,2012 David Vazquez
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

;;; User who invoked nilbot, target of nilbot ouptut and the user
;;; permissions respectively. They are dynamically bound when a
;;; privmsg is received.
(defvar *user*)
(defvar *recipient*)

;;; non-NIL if the server supports the capability IDENTIFY-MSG. We use
;;; this in order to be confident of the user and does not require
;;; authentication.
(defvar *identify-msg-p* nil)

;;; Output

;;; IRC Flood is often an impediment to produce useful output. We
;;; implement here an important feature: continuable output. Every
;;; response of nilbot command is truncated, but it is recorded in
;;; order to the user will be capable of continue the output with the
;;; ,more command.

(defvar *max-output-lines* 6)

;;; Number of seconds which a continuable output is recorded.
(defvar *max-pending-output-live* 300)

(defstruct (output-record (:constructor make-output-record (mark tail)))
  (timestamp (get-universal-time))
  (mark)
  (tail))

(defvar *pending-output*
  (make-hash-table :test #'equal))

(defun count-pending-output (to)
  (let ((recorded-ouptut (gethash to *pending-output*)))
    (if (null recorded-ouptut)
        0
        (length (cdr (output-record-mark recorded-ouptut))))))

(defun store-pending-output (to message)
  (let ((recorded-output (gethash to *pending-output*)))
    ;; Create a new recorded-output register if no one existed.
    (when (null recorded-output)
      (let ((cell (list ':message-mark)))
        (setf recorded-output (make-output-record cell cell))
        (setf (gethash to *pending-output*) recorded-output)))
    (with-slots (timestamp tail)
        recorded-output
      (let* ((new-cell (list message)))
        (setf (cdr tail) new-cell)
        (setf tail new-cell)))))

(defun reset-pending-output (to)
  (remhash to *pending-output*))

(defun finish-pending-output ()
  (do-hash-table (to output) *pending-output*
    ;; Clean deprecated output-records
    (when (> (- (get-universal-time) (output-record-timestamp output))
             *max-pending-output-live*)
      (reset-pending-output to))
    (loop for tail on (cdr (output-record-mark output))
          for head = (car tail)
          for count from 0
          until (eq head '---more---)
          do (irc:privmsg *irc* to head)
          finally
             (cond
               ((null tail)
                (reset-pending-output to)
                (return t))
               (t
                (unless (zerop count)
                  (irc:privmsg *irc* to "[more]"))
                (setf (cdr (output-record-mark output)) tail)
                (return nil))))))

(defun continue-pending-output (to)
  (let ((output (gethash to *pending-output*)))
    (when (and output (eq (cadr (output-record-mark output)) '---more---))
      (pop (cdr (output-record-mark output))))))


;;; High level functions.

(defun more (&optional (to *recipient*))
  (store-pending-output to '---more---))

;;; non-nil if the response must be immediate, instead of continuable.
(defvar *immediate-response-p* nil)

(defun response-to (to fmt &rest args)
  (cond
    (*immediate-response-p*
     (irc:privmsg *irc* to (apply #'format nil fmt args)))
    (t
     (when (zerop (mod (1+ (count-pending-output to)) *max-output-lines*))
       (more to))
     (store-pending-output to (apply #'format nil fmt args)))))

(defun response (fmt &rest args)
  (apply #'response-to *recipient* fmt args))



(defun action-to (to fmt &rest args)
  (irc::ctcp *irc* to (format nil "ACTION ~?" fmt args)))

(defun action (fmt &rest args)
  (apply #'action-to *recipient* fmt args))


(defun myself ()
  (irc:nickname (irc:user *irc*)))

(defun myselfp (str)
  (string= str (myself)))

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

(defun process-message (origin target message)
  ;; If the IDENTIFY-MSG is avalaible, we require the user is
  ;; identified in the services of the IRC server.
  (when *identify-msg-p*
    (if (char= (char message 0) #\+)
        (setq message (subseq message 1))
        (return-from process-message)))
  ;; Process the message
  (let (prefix)
    ;; Check if the message is a nilbot command
    (cond
      ((eql (char message 0) *default-prefix*)
       (setq prefix 1))
      ;; 'nilbot: ' messages
      ((let* ((mark (format nil "~a: " (irc:nickname (irc:user *irc*))))
              (posi (search mark message)))
         (and (integerp posi) (zerop posi)))
       (setq prefix (length (format nil "~a: " (irc:nickname (irc:user *irc*))))))
      ;; queries
      ((myselfp target)
       (setq prefix 0))
      (t
       (return-from process-message nil)))
    ;; Invoke the command
    (with-input-from-string (stream message :start prefix)
      (let ((cmd (parse-command stream))
            (arg (subseq (or (read-line stream nil) " ") 1)))
        (let ((*user* origin)
              (*recipient* (if (myselfp target) origin target)))
          (unless (permission= (user-permission *user*) "undesirable")
            (handler-case (run-command cmd arg)
              (simple-error (err)
                (let ((*immediate-response-p* t))
                  (apply #'response
                         (simple-condition-format-control err)
                         (simple-condition-format-arguments err)))))))))))

;;; Permissions functions

(deftype permission ()
  `(satisfies permissionp))

(defvar *permissions*
  #("undesirable" "nobody" "user" "admin"))

(defun permissionp (x)
  (find x *permissions* :test #'string-ci=))

(defun permission= (perm1 perm2)
  (declare (permission perm1 perm2))
  (string-ci= perm1 perm2))

(defun permission<= (perm1 perm2)
  (declare (permission perm1 perm2))
  (<= (position perm1 *permissions* :test #'string-ci=)
      (position perm2 *permissions* :test #'string-ci=)))

;;; Require PERM priviledge level.
(defun require-permission (perm)
  (unless (permission<= perm (user-permission *user*))
    (if (find (char perm 0) "aeiou")
        (error "You need be an ~a to do this." perm)
        (error "You need be a ~a to do this." perm))))



;;; This hashtable keeps the association between strings and handlers.
(defvar *command-handlers*
  (make-hash-table :test #'equal))

(defclass handler ()
  ((documentation
    :initarg :documentation
    :reader handler-documentation)
   (module
    :initform (let ((name (package-name *package*)))
                (if (string-prefix-p "NILBOT" name)
                    (and (< 6 (length name))
                         (eql (char name 6) #\.)
                         (subseq name 7))
                    (error "Defining a command from a non-nilbot package.")))
    :reader handler-module)
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
    :reader handler-aliases)
   ;; Taskbot will not discard the pending output if this is
   ;; non-nil. This is used to implement continuable output actually.
   (keep-last-output-p
    :initarg :keep-last-output-p
    :initform nil
    :reader handler-keep-last-output-p)))

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

(defun create-command (&rest initargs &key name &allow-other-keys)
  (setf (gethash name *command-handlers*)
        (apply #'make-instance 'handler :allow-other-keys t initargs)))

(defun create-command-alias (alias command)
  (let ((handler (find-handler command)))
    (if handler
        (setf (gethash alias *command-handlers*)
              (make-instance 'alias :handler handler))
        (error "The command `~a' does not exist." command))))


(defun run-command (command argument-line)
  (let ((handler (find-handler command)))
    (when (and handler (not (handler-keep-last-output-p handler)))
      (reset-pending-output *recipient*))
    (handler-case
        (cond
          ((null handler)
           (error "Unknown command"))
          ;; Commands with parsed arguments
          ((handler-parse-arguments-p handler)
           (with-input-from-string (stream argument-line)
             (apply (handler-function handler)
                    (parse-arguments stream))))
          ;; Command with raw argument
          ((not (handler-parse-arguments-p handler))
           (funcall (handler-function handler) argument-line)))
      ;; (type-error (error)
      ;;   (error "The datum ~a was expected to be of type ~a."
      ;;           (type-error-datum error)
      ;;           (type-error-expected-type error)))
      )
    (finish-pending-output)))


(defun arglist-unparsed-argument-p (arglist)
  (and (not (null arglist))
       (eq (car arglist) '&unparsed-argument)))

;;; Define a nilbot command. You can see nilbot-system.lisp for
;;; several examples of usage.
(defmacro define-command (name (&rest args) options &body code)
  (let ((documentation (cadr (assoc :documentation options)))
        (aliases (cdr (assoc :aliases options)))
        (permission (or (second (assoc :permission options)) "user"))
        (keep-last-output-p (second (assoc :keep-last-output-p options)))
        (fname (symbolize 'irc-handler- name)))
    (check-type documentation (or null string))
    (check-type permission permission)
    `(progn
       ;; Function handler
       (defun ,fname ,(if (arglist-unparsed-argument-p args) (cdr args) args)
         (block ,name
           ,(if permission `(require-permission ,permission))
           ,@code))
       ;; Register handler
       (create-command :name ,(string name)
                       :documentation ,documentation
                       :function ',fname
                       :parse-arguments-p ,(not (arglist-unparsed-argument-p args))
                       :permission ,permission
                       :keep-last-output-p ,keep-last-output-p)
       ;; Register aliases
       (dolist (alias ',aliases)
         (create-command-alias alias ,(string name))))))


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
          (error "~a is an invalid subcommand." ,subcommand-var))))))


;; nilbot-commands.lisp ends here
