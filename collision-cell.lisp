; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((matters :initform '()
            :initarg :matters
            :reader matters)))

(defmethod predict-collisions ((cell collision-cell) time)
  (with-slots (matters) cell
    (loop for part on matters
          for a = (car part)
          do (loop for b in (cdr part)
                   do (let ((c-time (print (aabb-collision-time a b))))
                        (when (and (typep c-time 'vector)
                                   (<= (print (a c-time)) 0))
                          (collision-update a b c-time)))))))

(defmethod apply-matter-physics ((cell collision-cell) time)
  (with-slots (matters) cell
    (dolist (matter matters)
      (predict-collisions cell time)
      (update-motion matter time)
      (displace matter))))