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
           (mapcar (lambda (p)
                     (+ (dot p axis) (dot (vec x y) axis))) points))))

(internal calc-axes)
(defmethod calc-axes ((polygon polygon))
  (flet ((left-normal (vec) (vec (- (y vec)) (x vec))))
    (with-slots (points axes) polygon
      (setf axes '())
      (loop with start = (car points)
         for (a b) on points until (null b)
         do (pushnew (left-normal (unit-vec a b))
                     axes :test #'unit-vec=)
         finally (pushnew (left-normal (unit-vec a start))
                          axes :test #'unit-vec=)))))
  
(defmethod initialize-instance :after ((polygon polygon) &key)
  (calc-axes polygon))

(defmethod collides-p ((a polygon) (b polygon))
  (dolist (axis (union (axes a) (axes b) :test #'unit-vec=) t)
    (let ((ap (project a axis))
          (bp (project b axis)))
      (format t "~&Axis: ~s; Limits A: ~s; Limits B: ~s."
              axis ap bp)
      (unless (overlaps-p ap bp)
        (return-from collides-p nil)))))