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

(uiop:define-package #:epap-web
  (:use #:common-lisp #:hunchentoot #:spinneret))

(in-package #:epap-web)

(defun web-app () 
  (start (make-instance 'easy-acceptor :port 80)))

(define-easy-handler (epap-root :uri "/") ()
  (with-html-string
    (:html 
     (:head (:title "Urbion"))
     (:body
      (:ul
       (:li (:a :href "/epap/COLD-MOUNTAIN" "Songs from Cold Mountain"))
       (:li (:a :href "/epap/SHUT-DOWN" "Shut down")))))))

(define-easy-handler (epap :uri "/epap") ()
  (with-html-string
    (:html
     (:head (:title "EPAP"))
     (:body
      (let* ((epap::*display-width* 1872)
             (epap::*display-height* 1404)
             (epap::*local-framebuffer*
               (make-array (list epap::*display-height*
                                 epap::*display-width*)
                           :element-type 'bit)))
        )
      (:img :src (format nil "data:image/png;base64,~S" ""))))))

(uiop:define-package #:epap-routes)
  
(defmacro expose-function (function-symbol)
  (let ((function-name (symbol-name function-symbol)))
    `(define-easy-handler
         (,(intern function-name (find-package :epap-routes))
          :uri ,(format nil "/epap/~a" function-name))
         ()
       (epap::for-real
         (,function-symbol)
         ,(format nil "~a~%" function-symbol)))))

(expose-function epap::start-display)
(expose-function epap::initialize-blank-display)
(expose-function epap::enter-sleep-mode)
(expose-function epap::shut-down)
(expose-function epap::cold-mountain)

(setf *catch-errors-p* nil)

(defun display-image ()
  (epap::display-image (png:decode (raw-post-data :want-stream t))))

(expose-function display-image)
