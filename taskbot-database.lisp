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

;;; Channel related functions

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

(defun db-create-user (name permission)
  (sqlite:execute-non-query *database* "
INSERT INTO users(nick, permission) VALUES(?, ?)" name permission))

(defun db-delete-user (name)
  (sqlite:execute-non-query *database* "
DELETE FROM users WHERE nick=?" name))

(defun db-list-users ()
  (mapcar #'car (sqlite:execute-to-list *database* "
SELECT nick FROM users")))

(defun db-update-user (oid nick permission)
  (sqlite:execute-non-query *database* "
UPDATE users SET nick=?, permission=? WHERE oid=?" nick permission oid))

(defun db-query-user (name)
  (sqlite:execute-one-row-m-v *database* "
SELECT oid,nick,permission FROM users WHERE nick=?
" name))



;;; Emacs and slime provides beautiful indentation so.
(defmacro deftable (name &body fields)
  `(let ((output (make-string-output-stream)))
     (format output "CREATE TABLE ~a(~{~(~a~) ~a~^, ~})" ',name ',fields)
     (sqlite:execute-non-query db (get-output-stream-string output))))

(defun %setup-schema ()
  (let ((pathname *database-pathname*))
    ;; If the database exists
    (when (probe-file pathname)
      (format t "The database `~a' exists." pathname)
      (cond
        ((yes-or-no-p "Are you sure you want to reset it?")
         (format t "Deleting `~a'~%" pathname)
         (delete-file pathname))
        (t
         (write-line "cancelling")
         (throw 'setup-quit nil))))
    (write-line "Creating database...")
    (sqlite:with-open-database (db pathname)
      (write-line "Creating table tasks...")
      (deftable tasks
        :id "INTEGER PRIMARY KEY ASC"
        :description "TEXT"
        :place "TEXT"
        :date "INTEGER"
        :author "TEXT")

      (write-line "Creating table users...")
      (deftable users
        :id "INTEGER PRIMARY KEY ASC"
        :nick "TEXT"
        :permission "TEXT")

      (write-line "Creating table channels...")
      (deftable channels
        :name "TEXT"))))


(defun %setup-data ()
  (with-database
    ;; Initialize with settings
    (let (admin)
      (write-string "Type the nickname of the first admin of taskbot: ")
      (setq admin (read-line))
      (write-line "flushing initial settings...")
      (db-create-user admin "admin"))))


;;; Setup the database schema and add the initial settings.
(defun setup ()
  (catch 'setup-quit
    (write-line "---")
    (%setup-schema)
    (write-line "---")
    (%setup-data)
    (write-line "done.")))

;;; taskbot-database.lisp ends here
