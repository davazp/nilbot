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
     (:aliases "HI" "HOLA"))
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


(defun format-help (docstring)
  (if (> (length docstring) 100)
      (subseq docstring 0 100)
      docstring))

(define-command help (&optional command)
    ((:documentation "Show documentation about a command."))
  (cond
    (command
     (let ((handler (find-handler command)))
       (let ((docstring (and handler (handler-documentation handler))))
         (if docstring
             (response "~a" (format-help docstring))
             (response "No documentation for the ~a command." command)))))
    (t
     (response "Avalaible commands: ~{~a~#[.~; and ~:;, ~]~}"
               (with-collect
                 (do-hash-table (name handler) *command-handlers*
                   (when (handlerp handler)
                     (collect name))))))))


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

(defun deaccumulate (n acc)
  (declare (type (integer 0 *) n))
  (let ((quotient (floor (/ n (first acc))) ))
    (if (endp (cdr acc))
        (list quotient)
        (cons (mod quotient (second acc))
              (deaccumulate quotient (cdr acc))))))

;;; Return a human-readable string representacion of SECONDS.
;;;
;;; The output string is composed by the components years, days,
;;; hours, minutes and seconds. All components could be ommited.  If
;;; PRECISSION is given, then it should be a integer which specify the
;;; number of components string will have. If ABBREV is non-nil then
;;; the name of the components will be abbreviated in order to
;;; procedure a more compact string.
(defun format-time (seconds &key (precission nil) (abbrev nil))
  (unless (zerop seconds)
    (let* ((long-names   '("second" "minute" "hour" "day" "year"))
           (abbrev-names '("s"      "m"      "h"    "d"   "y"))
           (component-names (if abbrev abbrev-names long-names))
           (output
            (loop with i = 0
                  for c in (reverse component-names)
                  for n in (reverse (deaccumulate seconds '(1 60 60 24 365)))
                  while (or (null precission) (< i precission))
                  unless (zerop n)
                  collect n and collect c and do (incf i))))
      (if abbrev
          (format nil "~{~d~a~^ ~}" output)
          (format nil "~{~d ~a~2:*~p~*~#[~;~; and ~:;, ~]~}" output)))))

(define-command uptime ()
    ((:documentation "Tell how long has taskbot been running."))
  (let ((seconds (- (get-universal-time) *uptime*)))
    (if (zerop seconds)
        (response "I have not been running!")
        (response "I have been running for ~a. (-:" (format-time seconds)))))


;;; taskbot-system.lisp ends here
