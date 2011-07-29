; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((matters :initform '()
            :initarg :matters
            :reader matters)))

(defmethod predict-collisions ((cell collision-cell))
  (flet ((update-nearest-time (time matter other)
           (let ((nearest-time (car (nearest-collision matter))))
             (when (or (null nearest-time)
                       (and (numberp (vec-a nearest-time))
                            (vec2a> nearest-time time)))
               (setf (nearest-collision matter) (cons time other))))))
    (with-slots (matters) cell
      (loop for part on matters
            for a = (car part)
            do (loop for b in (cdr part)
                     do (let ((time (aabb-collision-time a b)))
                          (if (or (eq (vec-a time) t)
                                  (eq (vec-b time) t))
                              (setf (nearest-collision a) (cons time b)
                                    (nearest-collision b) (cons time a))
                              (progn (update-nearest-time time a b)
                                     (update-nearest-time time b a)))))))))


(defmethod apply-matter-physics ((cell collision-cell) time)
  (with-slots (matters) cell
    (dolist (matter matters)
      (predict-collisions cell)
      (update-motion matter time)
      (displace matter))))