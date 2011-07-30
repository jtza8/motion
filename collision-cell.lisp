; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((matters :initform '()
            :initarg :matters
            :reader matters)))

;; (defmethod predict-collisions ((cell collision-cell))
;;   (with-slots (matters) cell
;;     (loop for part on matters
;;           for a = (car part)
;;           do (loop for b in (cdr part)
;;                    do (let ((time (aabb-collision-time a b)))
;;                         ))))))


(defmethod apply-matter-physics ((cell collision-cell) time)
  (with-slots (matters) cell
    (dolist (matter matters)
      (predict-collisions cell)
      (update-motion matter time)
      (displace matter))))