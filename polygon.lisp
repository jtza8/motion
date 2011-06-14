; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass polygon ()
  ((points :initform '()
           :initarg :points
           :accessor points)
   (axes :initform '()
         :reader axes)
   (projections :initform '()
                :reader projections)))

(defmethod project ((polygon polygon) (axis vec))
  (with-slots (points) polygon
    (apply #'min-max
           (mapcar (lambda (point)
                     (dot-product point axis))
                   points))))

(defmethod calc-axes ((polygon polygon))
  (with-slots (points axes) polygon
    (loop with start = (car points)
          for (a b) on points
          until (null b)
          collect (unit-vec a b) into result
          finally (return (setf axes (push (unit-vec start a) result))))))

(defmethod calc-projections ((polygon polygon))
  (with-slots (axes projections) polygon
    (setf projections
          (loop for axis in axes
                collect (cons axis (project polygon axis))))))

(defmethod collides-p ((a polygon) (b polygon))
  (with-slots (points) a
    ()))