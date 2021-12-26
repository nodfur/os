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

(uiop:define-package :epap-dev
  (:use :clim :clim-lisp))

(in-package :epap-dev)

(define-application-frame app ()
  ()
  (:pointer-documentation t)
  (:panes
   (app-pane :application
             :height 800 :width 600
             :display-function 'display-app)
   (app-interactor :interactor :height 200 :width 600))
  (:layouts
   (app-layout (vertically ()
                 app-pane
                 app-interactor))))

(define-app-command (quit :name t) ()
  (frame-exit *application-frame*))

(defun display-app (frame pane)
  (declare (ignore frame))
  (format pane "hello ~A" '(foo bar)))

(defun run ()
  (run-frame-top-level (make-application-frame 'app)))
