;;                                                               -*- Lisp -*-
;; nilbot-linkstore.lisp --
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

(defpackage :nilbot.linkstore
  (:use :cl :elephant :nilbot :nilbot.utils))
(in-package :nilbot)


(defvar *link-count* 0)

(defpclass link ()
  ((id
    :initarg :id
    :type integer
    :index t
    :initform (incf *link-count*))
   (url
    :initarg :url
    :type string
    :index t)
   (channel
    :initarg :channel
    :type string
    :index t)
   (date
    :initarg :date
    :type integer
    :index t
    :initform (get-universal-time))
   (author
    :initarg :author
    :type string
    :index t)
   (context
    :initarg :context
    :type string)))


(defun create-links ()
  (dotimes (var 2)
    (make-instance 'link :url (format nil "http://link~a.com" var) :author "raimon")))

(defun find-link (author)
  (get-instance-by-value 'link 'author author))

(defun all-links ()
  (get-instances-by-class 'link))

(defun match-string (key instance)
  (format t "~a"
	  (slot-value instance 'url)))

(defun find-links-by-substring (subs)
  (let* ((links (all-links))
	(len (length links)))
      (remove-if-not (lambda (x)
		       (search subs (slot-value x 'url)))
		     links)))

(defun create-link (url author context)
  (make-instance 'link :author author :url url :context context))

(define-command store-link (url)
    ((:documentation "store a link for future reference")
     (:aliases "S" "SL")
     (:permission "nobody"))
  (create-link url *user* "")
  (response (format nil "~a : link stored" *user*)))

(define-command list-links (&unparsed-argument substring)
    ((:documentation "finds links by substring")
     (:aliases "LL")
     (:permission "nobody"))
  (mapcar (lambda (link)
	    (response "~a: ~a" *user* (slot-value link 'url)))
	  (find-links-by-substring substring)))

(push 'look-for-link *receive-message-hook*)
(setq look-for-link '(process-message 'look-for-link))
(defun look-for-link (source target input)
  (when (search "http://" input)
    (do-matches-as-strings (match "(http://[^ ]+)" input)
      (create-link match source input))))


;; (in-package :nilbot.linkstore)
;; (define-command getlinks ()
;;     ()
;;   (let ((first-url (get-instance-by-value 'link))))
;;   (response "~a" url))
