; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* #v(0 (* 9.8 *ppm*)))

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
   (fixed :initform nil
          :initarg :fixed
          :accessor fixed)
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
      :accessor acceleration)
   (centroid :reader centroid)))

(defmethod initialize-instance :after ((polygon polygon) &key)
  (provide-events polygon :collision)
  (calc-axes polygon)
  (with-slots (centroid points) polygon
    (setf centroid (vec/ (reduce #'vec+ points) (float (length points))))))

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
            do (pushnew (left-normal (normalise (vec- b a)))
                        axes :test #'vec=)
            finally (pushnew (left-normal (normalise (vec- start a)))
                             axes :test #'vec=)))))
  
(defmethod calc-motion ((polygon polygon) time)
  (with-slots (fixed m s v a) polygon
    (when fixed
      (return-from calc-motion))
    (setf v (vec+ v (vec* a time) (vec* *gravity* time))
          s (vec* v time))
    ))

(defmethod overlap-poly ((a polygon) (b polygon) axis)
  (let ((ap (vec+ (project a axis) (dot (vec (x a) (y a)) axis)))
        (bp (vec+ (project b axis) (dot (vec (x b) (y b)) axis))))
    (overlap ap bp)))


(defmethod collide ((a polygon) (b polygon))
  (loop with min-overlap and min-axis
        for axis in (union (axes a) (axes b) :test #'vec=)
        for overlap = (overlap-poly a b axis)
        when (<= overlap 0.0) return #v(0.0 0.0)
        when (or (null min-overlap) (< overlap min-overlap))
          do (setf min-overlap overlap
                   min-axis axis)
        finally (return (vec* min-overlap (normalise (s a))))))

(defmethod correct-displacement ((polygon polygon) delta)
  (with-slots (s v a) polygon
    (setf s (vec+ s delta)
          v #v(0.0 0.0)
          a #v(0.0 0.0))))

(defmethod displace ((polygon polygon))
  (with-slots (fixed s x y) polygon
    (when fixed
      (return-from displace))
    (incf x (x s))
    (incf y (y s))
    (setf s #v(0 0))))