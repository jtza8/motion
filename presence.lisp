; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* #v(0 (* 9.8 *ppm*)))

(defclass presence (listenable listener)
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
      :accessor acceleration)))

(defmethod initialize-instance :after ((presence presence) &key)
  (provide-events presence :collision)
  (calc-axes presence))

(defmethod project ((presence presence) (axis vec))
  (with-slots (points) presence
    (apply #'min-max
           (mapcar (lambda (p)
                     (+ (dot p axis))) points))))

(internal calc-axes)
(defmethod calc-axes ((presence presence))
  (flet ((left-normal (vec) (vec (- (y vec)) (x vec))))
    (with-slots (points axes) presence
      (setf axes '())
      (loop with start = (car points)
            for (a b) on points until (null b)
            do (pushnew (left-normal (normalise (vec- b a)))
                        axes :test #'vec=)
            finally (pushnew (left-normal (normalise (vec- start a)))
                             axes :test #'vec=)))))
  
(defmethod calc-motion ((presence presence) time)
  (with-slots (fixed m s v a) presence
    (when fixed
      (return-from calc-motion))
    (setf v (vec+ v (vec* a time) (vec* *gravity* time))
          s (vec* v time))
    ))

(defmethod overlap-poly ((a presence) (b presence) axis)
  (let ((ap (vec+ (project a axis) (dot (vec (x a) (y a)) axis)))
        (bp (vec+ (project b axis) (dot (vec (x b) (y b)) axis))))
    (overlap ap bp)))


(defmethod collide ((a presence) (b presence))
  (loop with min-overlap and min-axis
        for axis in (union (axes a) (axes b) :test #'vec=)
        for overlap = (overlap-poly a b axis)
        when (<= overlap 0) return #v(0.0 0.0)
        when (or (null min-overlap) (< overlap min-overlap))
          do (setf min-overlap overlap
                   min-axis axis)
        finally (return (vec* min-overlap min-axis))))

(defmethod correct-displacement ((presence presence) delta)
  (with-slots (s v a) presence
    (setf s (vec+ s delta)
          v #v(0.0 0.0)
          a #v(0.0 0.0))))

(defmethod displace ((presence presence))
  (with-slots (fixed s x y) presence
    (when fixed
      (return-from displace))
    (incf x (x s))
    (incf y (y s))
    (setf s #v(0 0))))