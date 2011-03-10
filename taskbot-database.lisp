;;                                                               -*- Lisp -*-
;; taskbot-database.lisp --
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

(defvar *database*)

;;; Run CODE with the database at PATHNAME loaded.
(defmacro with-database (&body code)
  `(progn
     (unless (probe-file *database-pathname*)
       (error "Database `~a' does not exist." *database-pathname*))
     ;; We set *database* indeed of bound it in order to provide
     ;; access to the value *database* from the Slime REPL and,
     ;; therefore, all database function working.
     (setf *database* (sqlite:connect *database-pathname*))
     (unwind-protect
          (progn ,@code))
     (sqlite:disconnect *database*)
     (setf *database* nil)))


;;; This variable keeps database schema. They are defined with
;;; `define-table' in several locations across taskbot.
(defvar *database-schema*
  (make-hash-table))

(defun %define-table-fn (name fields)
  (lambda ()
    (let ((output (make-string-output-stream)))
      (format output "CREATE TABLE ~a(~{~(~a~) ~a~^, ~})" name fields)
      (sqlite:execute-non-query *database* (get-output-stream-string output)))))

(defmacro define-table (name &body fields)
  `(setf (gethash ',name *database-schema*)
         (%define-table-fn ',name ',fields)))


;;; Channel related functions

(define-table channels
  name "TEXT")

(defun db-create-channel (name)
  (sqlite:execute-non-query *database* "
INSERT INTO channels(name) VALUES (?)" name))

(defun db-delete-channel (name)
  (sqlite:execute-non-query *database* "
DELETE FROM channels WHERE name=?" name))

(defun db-list-channels ()
  (mapcar #'car (sqlite:execute-to-list *database* "
SELECT name FROM channels")))


;;; Users related functions

(define-table users
  id "INTEGER PRIMARY KEY ASC"
  nick "TEXT"
  permission "TEXT")

(defun db-create-user (name permission)
  (sqlite:execute-non-query *database* "
INSERT INTO users(nick, permission) VALUES(?, ?)" name permission))

(defun db-delete-user (name)
  (sqlite:execute-non-query *database* "
DELETE FROM users WHERE nick=?" name))

(defun db-list-users ()
  (mapcar #'car (sqlite:execute-to-list *database* "SELECT nick FROM users")))

(defun db-update-user (oid nick permission)
  (sqlite:execute-non-query *database* "
UPDATE users SET nick=?, permission=? WHERE oid=?" nick permission oid))

(defun db-query-user (name)
  (sqlite:execute-one-row-m-v *database* "
SELECT oid,nick,permission FROM users WHERE nick=?
" name))



(defun database-initialize-schema ()
  (write-line "Creating database...")
  (do-hash-table (name function) *database-schema*
    (format t "  ~(~a~) table~%" name)
    (funcall function)))

(defun database-initialize-data ()
  (let (admin channel)
    (write-string "Type the nickname of the first admin of taskbot: ")
    (finish-output)
    (setq admin (read-line))
    (write-string "Type a channel name in order to taskbot joins: ")
    (finish-output)
    (setq channel (read-line))
    (write-line "flushing initial settings...")
    (db-create-user admin "admin")
    (db-create-channel channel)))

;;; Setup the database schema and add the initial settings.
(defun setup ()
  (write-line "---")
  (let ((pathname *database-pathname*))
    (when (probe-file pathname)
      (format t "The database `~a' exists." pathname)
      (cond
        ((yes-or-no-p "Are you sure you want to reset it?")
         (format t "Deleting `~a'~%" pathname)
         (delete-file pathname))
        (t
         (write-line "cancelling")
         (return-from setup))))
    ;; Use sqlite:with-open-database indeed of with-database because
    ;; the second check if the database file exists.
    (sqlite:with-open-database (*database* pathname)
      (database-initialize-schema)
      (write-line "---")
      (database-initialize-data)
      (write-line "done."))))

;;; taskbot-database.lisp ends here
