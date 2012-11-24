;;                                                               -*- Lisp -*-
;; packages.lisp --
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

(defpackage :nilbot
  (:use :cl :elephant)
  (:export #:start
           #:stop
           ;; Channels
           #:add-channel
           #:list-channels
           #:delete-channel
           ;; Users
           #:user-nickname
           #:user-permissionn
           #:add-user
           #:list-users
           ;; Commands
           #:define-command
           #:&unparsed-argument
           #:subcommand-dispatch
           ;; Command context
           #:myself
           ;; Command responses
           #:*immediate-response-p*
           #:response-to
           #:response
           #:action-to
           #:action
           #:more))

;; packages.lisp ends here
