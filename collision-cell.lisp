; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((objects :initform '()
            :initarg :objects
            :accessor objects)))

(defmethod initialize-instance :after ((cell collision-cell) &key)
  (desire-events cell :before-frame (no-event-arg #'detect-collisions)))

(defmethod detect-collisions ((cell collision-cell))
  (with-slots (objects) cell
    (loop for subset on objects
          do (loop with a = (car subset)
                   for b in (cdr subset)
                   for delta = (overlap (presence a) (presence b))
                   unless (= delta 0.0)
                     do (progn
                          (setf (v a) #v(0 0)
                                (s a) 
                                (vec- (s a) (vec* delta (normalise (s a)))))
                          (when (< (magnitude (s a)) 1.0)
                            (setf (s a) #v(0.0 0.0)))
                          (setf (s b) #v(0 0) (v b) #v(0 0) (a b) #v(0 0))
                          (send-event a `(:matter-collision :a ,a :b ,b))
                          (send-event b `(:matter-collision :a ,b :b ,a)))))))