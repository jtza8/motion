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
   (axes :reader axes)
   (projections :reader projections)))

(defmethod initialize-instance :after ((poly poly) &key)
  (provide-events poly :collision)
  (update-axes poly)
  (update-projections poly))

(defmethod project ((poly poly) (axis vec))
  (with-slots (points) poly
    (apply #'min-max
           (mapcar (lambda (p)
                     (+ (dot p axis)))
                   points))))

(defmethod project-with-cache ((poly poly) (axis vec))
  (with-slots (projections) poly
    (or (cdr (assoc axis projections :test #'vec=))
        (project poly axis))))

(internal update-axes)
(defmethod update-axes ((poly poly))
  (with-slots (points axes) poly
    (setf axes '())
    (flet ((push-normal (a b)
             (let ((normalised (normalise (vec- b a))))
               (pushnew (vec (- (y normalised)) (x normalised))
                        axes :test #'vec=))))
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
  (loop with min-overlap
        for axis in (union (axes a) (axes b) :test #'vec=)
        for overlap = (overlap (vec+ (project-with-cache a axis)
                                     (dot (vec (x a) (y a)) axis))
                               (vec+ (project-with-cache b axis)
                                     (dot (vec (x b) (y b)) axis)))
        when (<= overlap 0.0) return 0.0
        when (or (null min-overlap) (< overlap min-overlap))
          do (setf min-overlap overlap)
        finally (return min-overlap)))