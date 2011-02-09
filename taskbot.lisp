;;                                                               -*- Lisp -*-
;; taskbot.lisp --
;;
;; Time-stamp: <2011-02-09 22:36:34 davazp>
;; Author: David Vazquez <davazp@es.gnu.org>
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

(defvar *version* '(0 1))
(defvar *uptime*)

;;; Global descriptor of the IRC server connection
(defvar *irc*)

(defun start (&key
              (nickname *default-irc-nickname*)
              (server   *default-irc-server*)
              (port     *default-irc-port*)
              (channels *default-irc-channels*))
  "Start taskbot."
  (flet ((run ()
           (setf *irc* (irc:connect :nickname nickname :server server :port port))
           (setf *uptime* (get-universal-time))
           (irc:add-hook *irc* 'irc:irc-privmsg-message 'privmsg-handler)
           (dolist (chan channels)
             (join  chan))
           (irc:read-message-loop *irc*)))
    ;; Use threads in order to keep the slime repl avalaible.
    #+sbcl (sb-thread:make-thread #'run)
    #-sbcl (run)))

(defun join (channel)
  "Join taskbot to a IRC channel."
  (irc:join *irc* channel))

(defun stop (&optional (message "taskbot says bye!"))
  "Stop taskbot."
  (irc:quit *irc* message)
  (values))

;; taskbot.lisp ends here
