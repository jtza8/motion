; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* #(0.0 (* 9.8 *ppm*)))

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
   (s :initform #(0 0)
      :accessor s
      :accessor displacement)
   (v :initarg :v
      :initform #(0 0)
      :accessor v
      :accessor velocity)
   (a :initarg :a
      :initform #(0 0)
      :accessor a
      :accessor acceleration)))

(defmethod initialize-instance :after ((matter matter) &key)
  (provide-events matter :matter-collision))

(defmethod update-motion ((matter matter) time)
  (with-slots (fixed m s v a) matter
    (when fixed
      (return-from update-motion))
    (setf v (vec2+ v (vec2* a time) (vec2* *gravity* time))
          s (vec2* v time))))

(defmethod displace ((matter matter))
  (with-slots (fixed presence s) matter
    (when fixed
      (return-from displace))
    (incf (x presence) (x s))
    (incf (y presence) (y s))
    (setf s #(0.0 0.0))))

(defun point-collision-time (s v a)
  (if (= a 0)
      (if (= v 0) 
          (if (zerop s) 0.0 :infinity)
          (float (/ s v)))
      (let* ((part-a (/ (* 2 (+ s (/ (* v v) (* 2 a)))) a))
             (part-b (if (minusp part-a)
                         (- (sqrt (abs part-a)))
                         (sqrt part-a))))
        (if (or (and (plusp s) (plusp v) (minusp a))
                (and (minusp s) (minusp v) (plusp a)))
            (- (- part-b) (/ v a))
            (- part-b (/ v a))))))

(defun segment-collision-time (s-a s-b v a)
  (let ((point-a (point-collision-time (- (a s-b) (b s-a)) v a)) point-b)
    (if (eq point-a :infinity)
        (return-from segment-collision-time #(:infinity :infinity))
        (setf point-b (point-collision-time (- (b s-b) (a s-a)) v a)))
    (when point-b
      (if (< point-b point-a)
          (vector point-b point-a)
          (vector point-a point-b)))))

(defmethod abs-point ((poly poly) point)
  (with-slots (x y) poly
    (vec2+ (vector x y) point)))

(defmethod aabb-collision-time ((a matter) (b matter))
  (with-slots ((poly-a presence)) a
    (with-slots ((poly-b presence)) b
      (let* ((a-a (abs-point poly-a (car (aabb poly-a))))
             (a-b (abs-point poly-a (cdr (aabb poly-a))))
             (b-a (abs-point poly-b (car (aabb poly-b))))
             (b-b (abs-point poly-b (cdr (aabb poly-b))))
             (v (vec2- (v a) (v b)))
             (accell (vec2- (a a) (a b)))
             (x-collision (segment-collision-time (vector (x a-a) (x a-b))
                                                  (vector (x b-a) (x b-b))
                                                  (x v)
                                                  (x accell)))
             (y-collision (segment-collision-time (vector (y a-a) (y a-b))
                                                  (vector (y b-a) (y b-b))
                                                  (y v)
                                                  (y accell))))
        (when (and x-collision y-collision)
          (intersect x-collision y-collision))))))

(defmethod collision-update ((a matter) (b matter) c-time)
  ())

(defmethod collision-update :after ((matter matter) (other matter) c-time)
  (send-event matter `(:matter-collision :a ,matter :b ,other :c-time ,c-time)))