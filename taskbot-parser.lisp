;;                                                               -*- Lisp -*-
;; taskbot-parser.lisp --
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

(defun whitespace-char-p (ch)
  (or (char= ch #\space)
      (char= ch #\tab)))

(defun skip-whitespace (stream)
  (read-until stream (complement #'whitespace-char-p) :eof-error-p nil)
  (values))

(defun parse-argument (stream)
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil)))
    (cond
      ;; No argument
      ((null ch)
       nil)
      ;; Integer
      ((digit-char-p ch)
       (let ((string (read-until stream #'whitespace-char-p :eof-error-p nil)))
         (parse-integer string)))
      ;; Simple string
      ((alpha-char-p ch)
       (read-until stream #'whitespace-char-p :eof-error-p nil))
      ;; Quote string
      ((char= ch #\')
       (prog2 (read-char stream)
           (read-until stream #\')
         (read-char stream)))
      ;; Double quote string
      ((char= ch #\")
       (prog2 (read-char stream)
           (read-until stream #\")
         (read-char stream))))))

(defun parse-arguments (stream)
  (loop for arg = (parse-argument stream)
        while arg
        collect arg))

(defun parse-command (stream)
  (read-until stream #'whitespace-char-p :eof-error-p nil))


;; taskbot-parser.lisp ends here
