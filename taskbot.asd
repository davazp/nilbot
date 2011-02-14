;;                                                               -*- Lisp -*-
;; taskbot.asd --
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

(defsystem :taskbot
  :name "Taskbot"
  :description "A task manager IRC Bot written in Common Lisp."
  :depends-on (:cl-irc :sqlite)
  :serial t
  :components
  ((:file "packages")
   (:file "config")
   (:file "utils")
   (:file "taskbot-database")
   (:file "taskbot")
   (:file "taskbot-parser")
   (:file "taskbot-commands")
   (:file "taskbot-system")
   (:file "taskbot-tracker")))

;; taskbot.asd ends here
