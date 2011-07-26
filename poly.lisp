; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass poly (listenable listener)
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)
   (points :initform '()
           :initarg :points
           :accessor points)
   (aabb :reader aabb)
   (axes :reader axes)
   (projections :reader projections)))

(defmethod initialize-instance :after ((poly poly) &key)
  (provide-events poly :collision)
  (update-axes poly)
  (update-projections poly)
  (update-aabb poly))

(defmethod update-aabb ((poly poly))
  (with-slots (aabb points) poly
    (setf aabb
          (loop for p in points
                collect (vec-x p) into x
                collect (vec-y p) into y
                finally (return
                          (cons (vector (apply #'min x) (apply #'max x))
                                (vector (apply #'min y) (apply #'max y))))))))

(defmethod project ((poly poly) axis)
  (with-slots (points) poly
    (loop for point in points
          for dot = (dot2 point axis)
          for min = dot then (min min dot)
          for max = dot then (max max dot)
          finally (return (vector min max)))))

(defmethod project-with-cache ((poly poly) axis)
  (with-slots (projections) poly
    (or (cdr (assoc axis projections :test #'axis2=))
        (project poly axis))))

(internal update-axes)
(defmethod update-axes ((poly poly))
  (with-slots (points axes) poly
    (setf axes '())
    (flet ((push-normal (a b)
             (let ((normalised (normalise (vec2- b a))))
               (pushnew (vector (- (vec-y normalised)) (vec-x normalised))
                        axes :test #'axis2=))))
      (loop with start = (car points)
            for (a b) on points until (null b)
            do (push-normal a b)
            finally (push-normal a start)))))

(internal update-projections)
(defmethod update-projections ((poly poly))
  (with-slots (projections axes) poly
    (setf projections
          (loop for axis in axes
                collect (cons axis (project poly axis))))))

(defmethod overlap ((a poly) (b poly))
  (loop
     with min-overlap and min-axis
     for axis in (union (axes a) (axes b) :test #'axis2=)
     for overlap = (seg-length
                    (overlap-vec2 (vec2+ (project-with-cache a axis)
                                         (dot2 (vector (x a) (y a)) axis))
                                  (vec2+ (project-with-cache b axis)
                                         (dot2 (vector (x b) (y b)) axis))))
     when (<= overlap 0.0) return 0.0
     when (or (null min-overlap) (< overlap min-overlap))
     do (setf min-overlap overlap
              min-axis axis)
     finally (return (values min-overlap min-axis))))