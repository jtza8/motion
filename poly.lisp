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
   (axes :reader axes)))

(defmethod initialize-instance :after ((poly poly) &key)
  (provide-events poly :collision)
  (update-axes poly))

(defmethod project ((poly poly) (axis vec))
  (with-slots (points) poly
    (apply #'min-max
           (mapcar (lambda (p)
                     (+ (dot p axis)))
                   points))))

(internal update-axes)
(defmethod update-axes ((poly poly))
  (flet ((left-normal (vec) (vec (- (y vec)) (x vec))))
    (with-slots (points axes) poly
      (setf axes '())
      (loop with start = (car points)
            for (a b) on points until (null b)
            do (pushnew (left-normal (normalise (vec- b a)))
                        axes :test #'vec=)
            finally (pushnew (left-normal (normalise (vec- start a)))
                             axes :test #'vec=)))))

(defmethod overlap ((a poly) (b poly))
  (loop with min-overlap
        for axis in (union (axes a) (axes b) :test #'vec=)
        for overlap = (overlap (vec+ (project a axis)
                                     (dot (vec (x a) (y a)) axis))
                               (vec+ (project b axis)
                                     (dot (vec (x b) (y b)) axis)))
        when (<= overlap 0.0) return #v(0.0 0.0)
        when (or (null min-overlap) (< overlap min-overlap))
          do (setf min-overlap overlap)
        finally (return min-overlap)))