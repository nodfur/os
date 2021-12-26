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

(uiop:define-package :knuth-plass
  (:use :common-lisp))

(in-package :knuth-plass)

(defparameter *paragraph-width* 68)

(defparameter *space-glue* '(glue 1 1 1))
(defparameter *paragraph-end-glue* '(glue 0 1000 0))
(defparameter *paragraph-end-penalty* '(forced-break))

(defun string-to-sequence (string)
  (let ((sequence
          (loop
            for word in (uiop:split-string string)
            appending `((box ,(length word) ,word)
                        ,*space-glue*))))
    (concatenate 'vector
                 (butlast sequence)
                 (list *paragraph-end-glue*)
                 (list *paragraph-end-penalty*))))

;; One thing that is not evident in the greedy algorithm is that we
;; are implicitly defining a score for how good each line break is;
;; and, a metric for how good the total set of line breaks we've
;; chosen are, together.

(defun normal-width (item)
  (ecase (car item)
    (box (cadr item))
    (glue (cadr item))
    (forced-break 0)))

(defun stretchability (item)
  (ecase (car item)
    (box 0)
    (glue (nth 2 item))
    (forced-break 0)))

(defun shrinkability (item)
  (ecase (car item)
    (box 0)
    (glue (nth 3 item))
    (forced-break 0)))

(defun minimum-width (item)
  (- (normal-width item) (shrinkability item)))

(defun maximum-width (item)
  (+ (normal-width item) (stretchability item)))

(defparameter *sequence* nil)

(defparameter +example+ "One thing that is not evident in the greedy algorithm is that we are implicitly defining a score for how good each line break is; and, a metric for how good the total set of line breaks we've chosen are, together.")

(defun minimum-width-between (a b)
  (loop for i from a below b
        for item = (svref *sequence* i)
        summing (minimum-width item)))

(defun normal-width-between (a b)
  (loop for i from a below b
        for item = (svref *sequence* i)
        summing (normal-width item)))

(defun total-shrinkability-between (a b)
  (loop for i from a below b
        for item = (svref *sequence* i)
        summing (shrinkability item)))

(defun total-stretchability-between (a b)
  (loop for i from a below b
        for item = (svref *sequence* i)
        summing (stretchability item)))

(defun line-parameters (a b)
  (loop for i from a below b
        for item = (svref *sequence* i)
        summing (normal-width item) into normal
        summing (stretchability item) into stretch
        summing (shrinkability item) into shrink
        finally
           (return (values (float normal)
                           (float stretch)
                           (float shrink)))))

(defun adjustment-ratio (a b)
  (multiple-value-bind (width stretch shrink) (line-parameters a b)
    (cond
      ((= *paragraph-width* width) 0)
      ((< *paragraph-width* width) (/ (- *paragraph-width* width) stretch))
      ((> *paragraph-width* width) (/ (- *paragraph-width* width) shrink)))))

(defun badness (a b)
  (let ((r (adjustment-ratio a b)))
    (if (< r -1)
        10000
        (* 100 (abs (* r r r))))))

(defun gluep (item)
  (eql 'glue (car item)))

;; (defun next-feasible-breakpoint (origin)
;;   (loop for i from origin below (length *sequence*)
;;         for item = (svref *sequence* i)
;;         unless (gluep item) return nil
;;         summing (minimum-width item) into minimum
;;         summing (maximum-width item) into maximum
;;         when (and (<= minimum *paragraph-width*)
;;                   (>= maximum *paragraph-width*))
;;           return i))

(defun feasible (origin index)
  (and
   (gluep (svref *sequence* index))
   (loop for i from origin below index
         for item = (svref *sequence* i)
         summing (minimum-width item) into minimum
         summing (maximum-width item) into maximum
         when (and (<= minimum *paragraph-width*)
                   (>= maximum *paragraph-width*))
           return i)))

(defun draw-item (item)
  (ecase (car item)
    (box (caddr item))
    (glue "  ")
    (forced-break "")))

(defun draw-line (a b)
  (apply #'concatenate 'string
         (loop
           for i from a below b
           collecting (draw-item (svref *sequence* i)))))

(defun algorithm (text)
  (let* ((*sequence* (string-to-sequence text))
         (memoization-table (make-hash-table :test #'equal))
         (parent-table (make-hash-table :test #'eql)))
    (labels ((best-break (i j)
               (format t "~A ~A~%" i j)
               (let ((memoized-value (gethash (cons i j) memoization-table)))
                 (or memoized-value
                     (let* ((vals (sort (loop for n from (- j 1) downto i
                                              do (format t "n ~A~%" n)
                                              when (gluep (svref *sequence* n))
                                                collect (cons (+ (badness n j) (best-break (+ n 1) j))
                                                              (+ n 1)))
                                        (lambda (x y)
                                          (< (car x) (car y)))))
                            (best-val (caar vals))
                            (best-idx (cdar vals)))
                       (prog1 best-val
                         (setf (gethash (cons i j) memoization-table) best-val)
                         (setf (gethash i parent-table) best-idx)))))))
      (progn
        (best-break 0 (length *sequence*))
        parent-table))))
