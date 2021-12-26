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

(uiop:define-package #:epap
  (:use #:common-lisp
        #:cffi
        #:babel
        #:zpng
        #:base64
        #:cl-ppcre
        #:spinneret))

(in-package :epap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic testing framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *current-test*)

(defmacro test (name &body body)
  `(let ((*current-test* ,name))
     ,@body))

(define-condition failed-equality-assertion (error)
  ((actual :initarg :actual :reader assertion-actual)
   (expected :initarg :expected :reader assertion-expected)
   (test :initform *current-test* :reader assertion-test))

  (:report (lambda (condition stream)
             (format stream "Test: ~A~%Expected: ~A~%  Actual: ~A"
                     (assertion-test condition)
                     (assertion-expected condition)
                     (assertion-actual condition)))))

(define-condition test-ok (condition) ())

(defmacro assert-equalp (actual expected)
  (let ((a (gensym))
        (e (gensym)))
    `(let ((,a ,actual)
           (,e ,expected))
       (if (equalp ,a ,e)
           (prog1 :ok (signal 'test-ok))
           (error 'failed-equality-assertion :expected ,e :actual ,a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dry run functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dry-run* t)
(defvar *dry-run-log* ())

(defmacro maybe-dry-run (&body body)
  `(if *dry-run*
       (setf *dry-run-log* (cons ,body *dry-run-log*))
       (progn ,@body)))

(defun log-dry-run (datum)
  (setf *dry-run-log* (cons datum *dry-run-log*)))

(defmacro defun-with-dry-run (name lambda-list &body body)
  (let ((args (remove '&key lambda-list)))
    `(defun ,name ,lambda-list
       (if *dry-run*
           (log-dry-run (append (list ',name) (list ,@args)))
           (progn ,@body)))))

(defmacro dry-run (&body body)
  `(let ((*dry-run* t)
         (*dry-run-log* ()))
     (values (progn ,@body)
             (nreverse *dry-run-log*))))

(defun no-dry-run ()
  (when *dry-run*
    (error "no frobbing on dry run")))

(defmacro for-real (&body body)
  `(let ((*dry-run* nil))
     ,@body))
