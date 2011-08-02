; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass matter (listenable)
  ((presence :initarg :presence
             :initform (error "All matter must have a presence.")
             :accessor presence)
   (nearest-collision :initform nil
                      :accessor nearest-collision)
   (m :initform 1
      :initarg :m
      :accessor mass
      :accessor m)
   (s :initform #(0.0 0.0)
      :accessor s
      :accessor displacement)
   (v :initarg :v
      :initform #(0.0 0.0)
      :accessor v
      :accessor velocity)
   (a :initarg :a
      :initform #(0.0 0.0)
      :accessor a
      :accessor acceleration)))

(defmethod initialize-instance :after ((matter matter) &key)
  (provide-events matter :matter-collision))

(defmethod update-motion ((matter matter) time)
  (with-slots (m s v a) matter
    (let ((collision (car (nearest-collision matter))))
      (when (and (vectorp collision) (null (vec-a collision)))
        (if (<= 0 (vec-a collision))
            (setf s #(0.0 0.0) v #(0.0 0.0) a #(0.0 0.0))))
      (setf v (vec2+ v (vec2* (vec2+ a *gravity*) time))
            s (vec2-m-to-px (vec2* v time))))))

(defmethod displace ((matter matter))
  (with-slots (fixed presence s) matter
    (incf (x presence) (vec-x s))
    (incf (y presence) (vec-y s))
    (setf s #(0.0 0.0))))

(defmethod aabb-collision-time ((one matter) (two matter))
  (macrolet ((abs-seg-in-m (coord aabb matter cons-component)
               `(vec2-px-to-m (vec2+ (,cons-component ,aabb)
                                     (,coord (presence ,matter))))))
    (let* ((aabb-one (aabb (presence one)))
           (aabb-two (aabb (presence two)))
           (v (vec2- (v one) (v two)))
           (a (vec2- (a one) (a two)))
           (seg-x (segment-collision-time
                   (abs-seg-in-m x aabb-one one car)
                   (abs-seg-in-m x aabb-two two car)
                   (vec-x v) (vec-x a)))
          seg-y)
     (unless (or (and (numberp (vec-b seg-x)) (not (minusp (vec-b seg-x))))
                 (vec-a seg-x))
       (return-from aabb-collision-time seg-x))
     (setf seg-y (segment-collision-time
                  (abs-seg-in-m y aabb-one one cdr)
                  (abs-seg-in-m y aabb-two two cdr)
                  (vec-y v) (vec-y a)))
     (collision-overlap seg-x seg-y))))

(defmethod collision-update ((a matter) (b matter) c-time)
  ())

(defmethod collision-update :after ((matter matter) (other matter) c-time)
  (send-event matter `(:matter-collision :a ,matter :b ,other :c-time ,c-time)))