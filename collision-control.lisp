; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

; Collision Control, just like Air Traffic Control, or Mission Control only not.

(in-package :motion)

(defclass collision-control (listener)
  ((polygons :initform '()
             :initarg :polygons
             :accessor polygons)))

(defmethod initialize-instance :after ((control collision-control) &key)
  (desire-events control :before-frame #'detect-collisions))

(defmethod detect-collisions ((control collision-control) &optional event)
  (declare (ignore event))
  (with-slots (polygons) control
    (loop for subset on polygons
          do (loop with a = (car subset)
                   for b in (cdr subset)
                   unless (vec= (collide a b) #v(0 0))
                     do (progn (send-event a `(:collision :a ,a :b ,b))
                               (send-event b `(:collision :a ,b :b ,a)))))))