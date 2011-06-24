; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 20 "Pixels per meter.")
(defparameter *gravity* #v(0 (* -9.8 *ppm*)))

(defclass polygon (listenable listener)
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)
   (points :initform '()
           :initarg :points
           :accessor points)
   (axes :reader axes)
   (m :initform 1
      :initarg :m
      :accessor mass
      :accessor m)
   (s :initform #v(0 0)
      :accessor s
      :accessor displacement)
   (v :initarg :v
      :initform #v(0 0)
      :accessor v
      :accessor velocity)
   (a :initarg :a
      :initform #v(0 0)
      :accessor a
      :accessor acceleration)))

(defmethod initialize-instance :after ((polygon polygon) &key)
  (provide-events polygon :collision)
  (calc-axes polygon))

(defmethod project ((polygon polygon) (axis vec))
  (with-slots (points) polygon
    (apply #'min-max
           (mapcar (lambda (p)
                     (+ (dot p axis))) points))))

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
  
(defmethod collide ((a polygon) (b polygon))
  (loop with min-overlap and min-axis
        for axis in (union (axes a) (axes b) :test #'unit-vec=)
        for ap = (vec+ (project a axis) (dot (vec (x a) (y a)) axis))
        for bp = (vec+ (project b axis) (dot (vec (x b) (y b)) axis))
        for overlap = (overlap ap bp)
        when (or (null min-overlap) (< overlap min-overlap))
          do (setf min-overlap overlap
                   min-axis axis)
        when (< overlap 0) return #v(0.0 0.0)
        finally (return (vec* min-overlap min-axis))))

(defmethod calc-motion ((polygon polygon) time)
  (with-slots (m s v a) polygon
    (setf v (vec+ v (vec* a time))
          s (vec* v time))))

(defmethod displace ((polygon polygon))
  (with-slots (s x y) polygon
    (incf x (x s))
    (incf y (y s))
    (setf s #v(0 0))))