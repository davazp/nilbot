;;                                                               -*- Lisp -*-
;; nilbot.lisp --
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
(in-package :nilbot)

(defvar *version* '(0 1))
(defvar *uptime*)

;;; Global descriptor of the IRC server connection
(defvar *irc*)
(defvar *nickname*)

;;; This is an internal interface of cl-irc, however we need cap
;;; support really if the server supports it.
(irc::create-irc-message-classes (:cap))
(unexport 'irc-cap-message)

(unless *store-controller*
  (open-store `(:CLSQL (:SQLITE3 ,(namestring *database-pathname*)))))

(defun start-and-wait (&key
                       (nickname *default-irc-nickname*)
                       (server   *default-irc-server*)
                       (port     *default-irc-port*)
                       password
                       channels)
  (if password
      (setf *irc* (irc:connect :nickname nickname :server server :port port))
      (setf *irc* (irc:connect :nickname nickname :server server :port port :password password)))
  (setf *uptime* (get-universal-time))
  (irc:add-hook *irc* 'irc:irc-privmsg-message 'privmsg-handler)
  (irc:add-hook *irc* 'irc-cap-message 'cap-handler)
  (irc:add-hook *irc* 'irc:irc-join-message 'join-handler)
  ;; Enable IDENTIFY-MSG capability if avalaible.
  (irc::send-irc-message *irc* :cap "req" "identify-msg")
  (mapc #'join channels)
  (dolist (chan (list-channels))
    (join chan))
  (irc:read-message-loop *irc*))

(defun start (&rest options &key &allow-other-keys)
  "Start nilbot."
  (flet ((run () (apply #'start-and-wait options)))
    ;; Use threads in order to keep the slime repl avalaible.
    #+sbcl (sb-thread:make-thread #'run)
    #-sbcl (run)))

(defun join (channel)
  "Join nilbot to a IRC channel."
  (irc:join *irc* channel))

(defun part (channel)
  "Part nilbot to a IRC channel."
  (irc:part *irc* channel))

(defun stop (&optional (message "bye!"))
  "Stop nilbot."
  (irc:quit *irc* message)
  (values))

;; nilbot.lisp ends here
