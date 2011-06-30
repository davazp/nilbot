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

(define-command version ()
    ((:documentation "Show the version of taskbot."))
  (response "taskbot ~{~a~^.~} running on ~a (~a)"
            *version*
            (lisp-implementation-type)
            (lisp-implementation-version)))

(define-command date ()
    ((:documentation "Tell the system date and time."))
  (let ((daynames #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
        (monthnames #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (multiple-value-bind (second minute hour date month year day light zone)
        (get-decoded-time)
      (declare (ignorable light))
      (response "~a ~a ~D ~2,'0D:~2,'0D:~2,'0D GMT~@D ~D"
                (elt daynames day)
                (elt monthnames (1- month))
                date
                hour
                minute
                second
                (+ (- zone) (if light 1 0))
                year))))

(define-command hello ()
    ((:documentation "Say Hi in several languages.")
     (:aliases "HI" "HOLA")
     (:permission "nobody"))
  (let ((msg '("Hola!"
               "Hi!"
               "Hello"
               "Salut"
               "Bonjour"
               "Hallo"
               "Guten Tag"
               "Moin moin"
               "Helo"
               "Xitsonga"
               "printf (\"Hello, world!\\n\");"
               "Goddag"
               "Hei"
               "Guten Tag"
               "Ciao, Buon giorno"
               "Hallo, Dag"
               "Hei, God dag"
               "Pozdravljeni!"
               "Merhaba")))
    (response "~a ~a" (random-element msg) *context-from*)))


(defun list-commands ()
  (with-collectors (commands)
    (do-hash-table (name handler) *command-handlers*
      (when (handlerp handler)
        (collect-commands name)))
    (sort commands #'alphabetically<=)))

(define-command commands ()
  ((:documentation "List the avalaible commands.")
   (:permission "nobody"))
  (flet (;; Check if COMMAND is avalaible to the user according to
         ;; the permissions settings.
         (avalaible-command-p (command)
           (let ((handler (find-handler command)))
             (permission<= (handler-permission handler) *context-permission*))))
    ;; List avalaible commands
    (response "Avalaible commands: ~{~a~#[.~; and ~:;, ~]~}"
              (remove-if-not #'avalaible-command-p (list-commands)))))


(defun command-docstring (command)
  (let ((handler (find-handler command)))
    (and handler
         (permission<= (handler-permission handler) *context-permission*)
         (handler-documentation handler))))

(define-command help (&optional command)
    ((:documentation "Show documentation about a command.")
     (:permission "nobody"))
  (check-type command (or null string))
  ;; Without arguments, it is a alias for `COMMANDS'.
  (when (null command)
    (irc-handler-commands)
    (return-from help))
  ;; Show documentation for COMMAND.
  (let ((docstring (command-docstring command)))
    (cond
      (docstring
       (with-input-from-string (in docstring)
         (response "~a" (read-line in))
         (when (peek-char nil in nil)
           (more))
         (loop for line = (read-line in nil) while line do (response "~a" line))))
      (t
       (response "No documentation for the ~a command." command)))))


(define-command apropos (&rest words)
    ((:documetation "Search in the documentation of the avalaible commands.")
     (:permission "nobody"))
  (dolist (w words)
    (check-type w string))
  (do-hash-table (command handler) *command-handlers*
    ;; Require it is a command (not an alias) and it is avalaible.
    (when (and (handlerp handler)
               (permission<= (handler-permission handler) *context-permission*))
      (let ((docstring (handler-documentation handler)))
        (when docstring
          (when (every (lambda (w) (search w docstring :test #'char-ci=)) words)
            (response "~a: ~a"
                      command
                      (with-input-from-string (in docstring)
                        (read-line in)))))))))


(define-command machine ()
    ((:documentation "Show information about the machine."))
  (response "Hardware: ~a (~a)"
            (machine-version)
            (machine-type)))

(define-command software ()
    ((:documentation "Show information about the software."))
  (response "Software: ~a ~a"
            (software-type)
            (software-version)))

(define-command uptime ()
    ((:documentation "Tell how long has taskbot been running."))
  (let ((seconds (- (get-universal-time) *uptime*)))
    (if (zerop seconds)
        (response "I have not been running!")
        (response "I have been running for ~a. (-:" (format-time seconds)))))


(define-command join (chan1 &rest others)
    ((:documentation "Add channel to the channel-list of taskbot.")
     (:permission "admin"))
  (let ((channels (cons chan1 others)))
    (dolist (channel channels)
      (join channel)
      (add-channel channel))
    (if (singlep channels)
      (response "~a joined to ~a channel." (nickname) (car channels))
      (response "~a joined to ~{~a~#[~; and ~;, ~]~} channels." (nickname) channels))))

(define-command part (chan1 &rest channels)
    ((:documentation "Delete channel from the channel-list of taskbot.")
     (:permission "admin"))
  (let ((channels (cons chan1 channels)))
    (dolist (channel channels)
      (part channel)
      (delete-channel channel))
    (if (singlep channels)
        (response "~a parted from ~a channel." (nickname) (car channels))
        (response "~a parted from ~{~a~#[~; and ~;, ~]~} channels." (nickname) channels))))

(define-command channels ()
    ((:documentation "Show the the channel-list of taskbot.")
     (:permission "admin"))
  (let ((list (list-channels)))
    (if (null list)
        (response "~a is not in any channel yet." (nickname))
        (response "~a is in ~{~a~#[.~; and ~:;, ~]~}"
                  (nickname)
                  (mapcar #'channel-name list)))))


(define-command user (subcommand &rest args)
    ((:documentation "Manage list of users.
USER ADD <nickname> [permission]
USER APPPOINT <nickname> <permission>
")
     (:permission "admin"))
  (subcommand-dispatch subcommand args
    (("add" user &optional (perm "user"))
     (check-type user string)
     (check-type perm permission)
     (add-user user perm)
     (response "user added."))
    (("appoint" user new-permission)
     (check-type user string)
     (check-type new-permission permission)
     (let ((user (find-user user)))
       (if (not user)
           (add-user user new-permission)
           (setf (user-permission user) new-permission))
       (if (find (char new-permission 0) "aeiou")
           (response "~a is an ~a now" user new-permission)
           (response "~a is a ~a now" user new-permission))))))


(define-command ban (user)
    ((:documentation "Ban an user.")
     (:permission "admin"))
  (check-type user string)
  (let ((user (find-user user)))
    (if user
        (if (permission= (user-permission user) "admin")
            (%error "You cannot ban an admin.")
            (setf (user-permission user) "undesirable"))
        (add-user user "undesirable")))
  (response "~a is an undesirable now" user))


(define-command whois (name)
    ((:documentation "Print information about an user."))
  (let ((user (find-user name)))
    (if (not user)
        (response "User ~a does not exist." name)
        (if (find (char (user-permission user) 0) "aeiou")
            (response "~a is an ~a." (user-nickname user) (user-permission user))
            (response "~a is a ~a."  (user-nickname user) (user-permission user))))))

(define-command whoami ()
    ((:documentation "Print information about you."))
  (irc-handler-whois *context-from*))

(define-command bye ()
    ((:documentation "Quit taskbot.")
     (:permission "admin"))
  (stop))

(define-command more ()
    ((:documentation "Show pending output")
     (:keep-last-output-p t))
  (continue-pending-output *context-to*))

;;; taskbot-system.lisp ends here
