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
                (elt monthnames month)
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
  (cond
    (command
     (let ((docstring (command-docstring command)))
       (cond
         (docstring
          (if (> (length docstring) 100)
              (response "~a" (subseq docstring 0 100))
              (response "~a" docstring)))
         (t
          (response "No documentation for the ~a command." command)))))
    (t
     (irc-handler-commands))))


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


(define-command join (channel)
    ((:documentation "Add channel to the channel-list of taskbot.")
     (:permission "admin"))
  (response "~a channel added." channel)
  (join channel)
  (db-create-channel channel))

(define-command part (channel)
    ((:documentation "Delete channel from the channel-list of taskbot.")
     (:permission "admin"))
  (response "~a channel removed." channel)
  (part channel)
  (db-delete-channel channel))

(define-command channels ()
    ((:documentation "Show the the channel-list of taskbot.")
     (:permission "admin"))
  (let ((list (db-list-channels)))
    (if (null list)
        (response "taskbot is not in any channel yet.")
        (response "taskbot is in ~{~a~#[.~; and ~:;, ~]~}" list))))


(define-command user (subcommand &rest args)
    ((:documentation "Manage list of users.
USER ADD <nickname> [permission]
USER APPPOINT <nickname> <permission>
")
     (:permission "admin"))
  (subcommand-dispatch subcommand args
    (("add" user &optional (perm "user"))
     (cond
       ((db-query-user user)
        (response "user already exists."))
       (t
        (unless (permissionp perm)
          (%error "~a is not a permission level." perm))
        (db-create-user user perm)
        (response "user added."))))
    (("appoint" user new-permission)
     (check-type user string)
     (check-type new-permission permission)
     (multiple-value-bind (oid nick perm)
         (db-query-user user)
       (if (not oid)
           (db-create-user user perm)
           (db-update-user oid nick new-permission))
       (if (find (char perm 0) "aeiou")
           (response "~a is an ~a now" user new-permission)
           (response "~a is a ~a now" user new-permission))))))


(define-command ban (user)
    ((:documentation "Ban an user.")
     (:permission "admin"))
  (multiple-value-bind (oid nick perm)
      (db-query-user user)
    (declare (ignorable nick))
    (cond
      ((not oid)
       (db-create-user user "undesirable"))
      (t
       (when (string= perm "admin")
         (%error "You cannot ban an admin."))
       (db-update-user oid user "undesirable")))
    (response "~a is an undesirable now" user)))


(define-command whois (user)
    ((:documentation "Print information about an user."))
  (let ((oid (db-query-user user))
        (perm (get-user-permission user)))
    (if (find (char perm 0) "aeiou")
        (response "~a ~@[(#~d) ~]is an ~a." user oid perm)
        (response "~a ~@[(#~d) ~]is a ~a."  user oid perm))))

(define-command whoami ()
    ((:documentation "Print information about you."))
  (irc-handler-whois *context-from*))

(define-command sql (&unparsed-argument sentence)
    ((:documentation "Execute a sqlite sentence. WARNING: This is a dangerous command.")
     (:permission "admin"))
  (handler-case
      (dolist (result-line (sqlite:execute-to-list *database* sentence))
        (response "~{~a~^, ~}" result-line))
    (sqlite:sqlite-error (error)
      (%error "sqlite error: ~?"
              (simple-condition-format-control error)
              (simple-condition-format-arguments error)))))

(define-command bye ()
    ((:documentation "Quit taskbot.")
     (:permission "admin"))
  (stop))

(define-command more ()
    ((:documentation "Show pending output")
     (:keep-last-output-p t))
  (let ((finished (continue-pending-output *context-to*)))
    (when finished
      (immediate-response "[no more]"))))

;;; taskbot-system.lisp ends here
