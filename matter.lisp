; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* #v(0.0 (* 9.8 *ppm*)))

(defclass matter (listenable)
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
  (provide-events matter :matter-collision))

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

(defun point-collision-time (s v a)
  (if (= a 0)
      (if (= v 0) nil (float (/ s v)))
      (let ((part-a (/ (* 2 (+ s (/ (* v v) (* 2 a)))) a)))
        (if (minusp part-a) nil
            (- (sqrt part-a) (/ v a))))))

(defun segment-collision-time (s-a s-b v a)
  (let ((point-a (point-collision-time (- (a s-b) (b s-a)) v a)) point-b)
    (if point-a
        (setf point-b (point-collision-time (- (b s-b) (a s-a)) v a))
        (return-from segment-collision-time nil))
    (when point-b
      (vec point-a point-b))))

(defmethod aabb-collision-time ((a matter) (b matter))
  (macrolet ((time-segment (part carcdr)
               `(segment-collision-time (vec+ (,part (presence a))
                                              (,carcdr aabb-a))
                                        (vec+ (,part (presence b))
                                              (,carcdr aabb-b))
                                        (vec- (,part (v a)) (x (v b)))
                                        (vec- (,part (a a)) (x (a b))))))
    (let ((aabb-a (aabb (presence a)))
          (aabb-b (aabb (presence b))))
      (intersect (time-segment x car) (time-segment y cdr)))))

(defmethod collision-update ((a matter) (b matter) delta axis)
  (with-slots ((s-a s) (v-a v)) a
    (with-slots ((s-b s) (v-b v)) b
      (setf v-a #v(0.0 0.0)
            v-b #v(0.0 0.0))
      (cond ((and (fixed a) (fixed b)) nil)
            ((fixed a) (setf s-b (vec- s-b (vec* delta (normalise s-b)))))
            (t (setf s-a (vec- s-a (vec* delta (normalise s-a))))))
      (dolist (s (list s-a s-b))
        (when (< (magnitude s) 1.0)
          (setf s #v(0.0 0.0)))))))

(defmethod collision-update :after ((matter matter) (other matter) delta r-axis)
  (send-event matter `(:matter-collision :a ,matter :b ,other :r-axis ,r-axis)))