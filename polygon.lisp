; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass polygon ()
  ((points :initform '()
           :initarg :points
           :accessor points)))

(defmethod project ((polygon polygon) (axis vec))
  (with-slots (points) polygon
    (apply #'min-max
           (mapcar (lambda (point)
                     (dot-product point axis))
                   points))))

;; (defmethod collides-p ((a polygon) (b polygon))
;;   (with-slots (points) a