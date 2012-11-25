;;                                                               -*- Lisp -*-
;; nilbot-channels.lisp --
;;
;; Copyright (C) 2012 David Vázquez Púa
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

(defpclass channel ()
  ((name
    :initarg :name
    :type string
    :reader channel-name
    :index t)))

(defun find-channel (name)
  (get-instance-by-value 'channel 'name name))

(defun add-channel (name)
  (if (find-channel name)
      (error "Channel exists.")
      (create-instance 'channel :name name))
  (values))

(defun delete-channel (name)
  (let ((channel (find-channel name)))
    (when channel
      (drop-instance channel)))
  (values))

(defun list-channels ()
  (mapcar #'channel-name (get-instances-by-class 'channel)))

;; nilbot-channels.lisp ends here
