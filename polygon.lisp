; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass polygon ()
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)
   (points :initform '()
           :initarg :points
           :accessor points)
   (axes :reader axes)))

(defmethod project ((polygon polygon) (axis vec))
  (with-slots (points x y) polygon
    (apply #'min-max
           (mapcar (lambda (p) (dot (vec+ (vec x y) p) axis))
                   points))))

(internal calc-axes)
(defmethod calc-axes ((polygon polygon))
  (with-slots (points axes) polygon
    (setf axes '())
    (loop with start = (car points)
          for (a b) on points until (null b)
          do (pushnew (unit-vec a b) axes :test #'unit-vec=)
          finally (pushnew (unit-vec a start) axes :test #'unit-vec=))))

(defmethod initialize-instance :after ((polygon polygon) &key)
  (calc-axes polygon))

(defmethod collides-p ((a polygon) (b polygon))
  (dolist (axis (union (axes a) (axes b) :test #'unit-vec=) nil)
    (when (overlaps-p (project a axis) (project b axis))
      (return-from collides-p t))))