;;
;; Copyright (C) 2021  Restless Hypermedia, Inc.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(defsystem "epap"
  :depends-on ("cffi"
               "babel"
               "zpng"
               "cl-base64"
               "cl-ppcre"
               "trivia"
               "parenscript"
               "spinneret"
               "css-lite"
               "hunchentoot"
               "iterate"
               "printv")
  :serial t
  :components ((:file "00-basic")
               (:file "01-foreign")
               (:file "02-bcm2385")
               (:file "03-it8591")
               (:file "04-glyph")
               (:file "05-text")
               (:file "06-poetry")
               (:file "07-paragraph")
               (:file "08-browser")
               (:file "09-openai")
               (:file "10-tex")
               (:file "11-urbion")
               ;; (:file "99-chronicle")
               )
  )
