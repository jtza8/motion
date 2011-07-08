; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((objects :initform '()
            :initarg :objects
            :accessor objects)))

(defmethod detect-collisions ((cell collision-cell))
  (with-slots (objects) cell
    (loop for subset on objects
          do (loop with a = (car subset) and delta and axis
                   for b in (cdr subset)
                   do (setf (values delta axis)
                            (overlap (presence a) (presence b)))
                   unless (= delta 0.0)
                     do (collision-update a b delta axis)))))