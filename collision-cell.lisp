; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((matters :initform '()
            :initarg :matters
            :reader matters)
   (expired-matters :initform '())
   (collisions :initform '())))

(defstruct (collision-row (:constructor make-long-collision-row)
                          (:conc-name cr-))
  time a b s-a s-b v-a v-b a-a a-b)

(defun make-collision-row (&key time a b)
  (make-long-collision-row :time time :a a :b b
                           :s-a (s a) :s-b (s b)
                           :v-a (v a) :v-b (v b)
                           :a-a (a a) :a-b (a b)))

(internal cons-hash)
(defun cons-hash (car cdr)
  (min (sxhash (cons car cdr))
       (sxhash (cons cdr car))))

(defmethod initialize-instance :after ((cell collision-cell) &key)
  (with-slots (matters expired-matters) cell
    (setf expired-matters matters)
    (update-expired-matters cell)))

(defmethod add-matter ((cell collision-cell) (matter matter))
  (with-slots (matters expired-matters) cell
    (pushnew matter matters)
    (pushnew matter expired-matters)))

(defmethod matter-in-collision-row-p ((matter matter) collision)
  (or (eq matter (cr-a collision))
      (eq matter (cr-b collision))))

(defmethod query-collision ((cell collision-cell) (a matter) (b matter)
                            &optional override-cache)
  (with-slots (collisions) cell
    (let ((result (assoc (cons-hash a b) collisions :test #'=)))
      (when (or (null result) override-cache)
        (setf result (cons (cons-hash a b)
                           (make-collision-row :time (aabb-collision-time a b)
                                               :a a :b b)))
        (push result collisions))
      (cdr result))))

(defmethod query-possible-collisions ((cell collision-cell) (matter matter))
  (with-slots (collisions) cell
    (loop for (nil . collision) in collisions
          when (matter-in-collision-row-p matter collision)
            collect (print collision))))

(defmethod query-nearest-collision ((cell collision-cell) (matter matter))
  (let ((collisions (query-possible-collisions cell matter)))
    (loop with nearest
          for collision in collisions
          when (and (numberp (cr-time collision))
                    (> (cr-time nearest) (cr-time collision)))
            do (setf nearest collision)
          finally (return nearest))))

(defmethod update-expired-matters ((cell collision-cell))
  (with-slots (expired-matters) cell
    (loop for part on expired-matters
          for a = (car part)
          do (loop for b in (cdr part)
                   do (query-collision cell a b t)))
    (setf expired-matters '())))

(defmethod apply-matter-physics ((cell collision-cell) time)
  (with-slots (matters) cell
    (update-expired-matters cell)
    (dolist (matter matters)
      (update-motion matter time))
    (dolist (matter matters)
      (displace matter))))