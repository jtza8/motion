; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((polygons :initform '()
             :initarg :polygons
             :accessor polygons)))

(defmethod initialize-instance :after ((cell collision-cell) &key)
  (desire-events cell :before-frame (no-event-arg #'detect-collisions)))

(defmethod detect-collisions ((cell collision-cell))
  (with-slots (polygons) cell
    (loop for subset on polygons
          do (loop with a = (car subset)
                   for b in (cdr subset)
                   for delta = (collide a b)
                   unless (vec= delta #v(0 0))
                     do (progn
                          (setf (v a) #v(0 0)
                                (s a) 
                                (vec- (s a) delta))
                          (when (< (magnitude (s a)) 1.0)
                            (setf (s a) #v(0.0 0.0)))
                          (setf (s b) #v(0 0) (v b) #v(0 0) (a b) #v(0 0))
                          (send-event a `(:collision :a ,a :b ,b))
                          (send-event b `(:collision :a ,b :b ,a)))))))