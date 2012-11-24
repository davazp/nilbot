;;                                                               -*- Lisp -*-
;; nilbot.asd --
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

(defsystem :nilbot
  :name "nilbot"
  :description "An IRC Bot written in Common Lisp."
  :depends-on (:cl-irc :elephant :sqlite)
  :serial t
  :components
  ((:file "packages")
   (:file "config")
   (:file "utils")
   (:file "nilbot")
   (:file "nilbot-parser")
   (:file "nilbot-users")
   (:file "nilbot-channels")
   (:file "nilbot-commands")
   (:file "nilbot-notification")
   (:file "nilbot-system")
   (:module "contrib"
            :components
            ((:file "nilbot-tracker")))))

;; nilbot.asd ends here
