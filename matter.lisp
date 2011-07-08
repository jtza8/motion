; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* #v(0.0 (* 9.8 *ppm*)))

(defclass matter (listenable listener)
  ((presence :initarg :presence
             :initform (error "All matter must have a presence.")
             :accessor presence)
   (fixed :initform nil
          :initarg :fixed
          :accessor fixed)
   (m :initform 1
      :initarg :m
      :accessor mass
      :accessor m)
   (s :initform #v(0 0)
      :accessor s
      :accessor displacement)
   (v :initarg :v
      :initform #v(0 0)
      :accessor v
      :accessor velocity)
   (a :initarg :a
      :initform #v(0 0)
      :accessor a
      :accessor acceleration)))

(defmethod initialize-instance :after ((matter matter) &key)
  (provide-events matter :matter-collision)
  (desire-events matter :matter-collision #'matter-collision-handler)
  (subscribe matter matter :matter-collision))

(defmethod update-motion ((matter matter) time)
  (with-slots (fixed m s v a) matter
    (when fixed
      (return-from update-motion))
    (setf v (vec+ v (vec* a time) (vec* *gravity* time))
          s (vec* v time))))

(defmethod displace ((matter matter))
  (with-slots (fixed presence s) matter
    (when fixed
      (return-from displace))
    (incf (x presence) (x s))
    (incf (y presence) (y s))
    (setf s #v(0.0 0.0))))

(defmethod matter-collision-handler ((matter matter) event)
  (with-event-keys (a b) event
    ))
    ;; (setf (v a) )))