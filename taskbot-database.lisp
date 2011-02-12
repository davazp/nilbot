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
     (sqlite:with-open-database (*database* *database-pathname*)
       ,@code)))

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



;;; Emacs and slime provides beautiful indentation so.
(defmacro deftable (name &body fields)
  `(let ((output (make-string-output-stream)))
     (format output "CREATE TABLE ~a(~{~(~a~) ~a~^, ~})" ',name ',fields)
     (sqlite:execute-non-query db (get-output-stream-string output))))

;;; Setup the database schema and add the initial settings.
(defun setup ()
  (let ((pathname *database-pathname*))
    (write-line "---")
    ;; If the database exists
    (when (probe-file pathname)
      (format t "The database `~a' exists." pathname)
      (cond
        ((yes-or-no-p "Are you sure you want to reset it?")
         (format t "Deleting `~a'~%" pathname)
         (delete-file pathname))
        (t
         (write-line "cancelling")
         (return-from setup))))
    (write-line "Creating database...")
    (sqlite:with-open-database (db pathname)
      ;; Creating schema

      (write-line "Creating table tasks...")
      (deftable tasks
        :id "INTEGER PRIMARY KEY ASC"
        :description "TEXT"
        :place "TEXT"
        :date "INTEGER"
        :author "TEXT")

      (write-line "Creating table users...")
      (deftable users
        :nick "TEXT")

      (write-line "Creating table channels...")
      (deftable channels
        :name "TEXT")

      (write-line "---")
      ;; Initialize with settings
      (let (admin)
        (write-string "Type the nickname of the first admin of taskbot: ")
        (setq admin (read-line))
        (write-line "flushing initial settings...")
        (sqlite:execute-non-query db "INSERT INTO users (nick) VALUES (?)" admin))

      (write-line "done."))))

;;; taskbot-database.lisp ends here
