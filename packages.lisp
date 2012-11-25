;;                                                               -*- Lisp -*-
;; packages.lisp --
;;
;; Copyright (C) 2009,2011,2012 David Vazquez
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

(defpackage :nilbot.utils
  (:use :cl)
  (:export #:symbolize
           #:with-gensyms
           #:with-collectors
           #:with-collect
           #:define-predicate-type
           #:split-string
           #:concat
           #:join-strings
           #:mklist
           #:singlep
           #:unlist
           #:read-until
           #:required-arg
           #:do-hash-table
           #:random-element
           #:alphabetically<=
           #:char-ci=
           #:string-ci=
           #:truncate-string
           #:fill-string
           #:format-time
           #:create-instance
           #:string-prefix-p))

(defpackage :nilbot
  (:use :cl :elephant :nilbot.utils)
  (:export #:start
           #:stop
           ;; Channels
           #:add-channel
           #:list-channels
           #:delete-channel
           ;; Users
           #:add-user
           #:list-users
           #:user-permission
           ;; Commands
           #:define-command
           #:&unparsed-argument
           #:subcommand-dispatch
           ;; Command context
           #:*user*
           #:*recipient*
           #:myself
           ;; Command responses
           #:*immediate-response-p*
           #:response
           #:response-to
           #:more
           #:action
           #:action-to
           #:notificate
           #:notificate-to))


;; packages.lisp ends here
