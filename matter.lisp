; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *ppm* 300 "Pixels per meter.")
(defparameter *gravity* (vector 0.0 (* 9.8 *ppm*)))

(defclass matter (listenable)
  ((presence :initarg :presence
             :initform (error "All matter must have a presence.")
             :accessor presence)
   (fixed :initform nil
          :initarg :fixed
          :accessor fixed)
   (nearest-collision :initform nil
                      :accessor nearest-collision)
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
    (if (or (eq (vec-a (car (nearest-collision matter))) t)
            (eq (vec-b (car (nearest-collision matter))) t))
        (setf s #(0.0 0.0) v #(0.0 0.0) a #(0.0 0.0))
        (setf v (vec2+ v (vec2* a time) (vec2* *gravity* time))
              s (vec2* v time)))))

(defmethod displace ((matter matter))
  (with-slots (fixed presence s) matter
    (when fixed
      (return-from displace))
    (incf (x presence) (vec-x s))
    (incf (y presence) (vec-y s))
    (setf s #(0.0 0.0))))

(declaim (inline point-collision-time))
(defun point-collision-time (s v a)
  (if (= a 0)
      (if (= v 0) 
          (if (zerop s) 0.0 nil)
          (float (/ s v)))
      (let* ((part-a (/ (* 2 (+ s (/ (* v v) (* 2 a)))) a))
             (part-b (if (minusp part-a)
                         (- (sqrt (abs part-a)))
                         (sqrt part-a))))
        (if (or (and (plusp s) (plusp v) (minusp a))
                (and (minusp s) (minusp v) (plusp a)))
            (- (- part-b) (/ v a))
            (- part-b (/ v a))))))

(declaim (inline segment-collision-time))
(defun segment-collision-time (s-a s-b v a)
  (let (t-a t-b)
    (if (and (zerop v) (zerop a))
        (setf t-a (vec2-overlap-p s-a s-b)
              t-b (not t-a))
        (progn (setf t-a (point-collision-time (- (vec-a s-b) (vec-b s-a)) v a)
                     t-b (point-collision-time (- (vec-b s-b) (vec-a s-a)) v a))
               (unless (< t-a t-b)
                 (return-from segment-collision-time (vector t-b t-a)))))
    (vector t-a t-b)))

(declaim (inline collision-overlap))
(defun collision-overlap (one two)
  (if (and (numberp (vec-a one)) (numberp (vec-a two)))
      (overlap-vec2 one two)
      (if (null (vec-b one)) one two)))

(defmethod aabb-collision-time ((one matter) (two matter))
  (let* ((aabb-one (aabb (presence one)))
         (aabb-two (aabb (presence two)))
         (v (vec2- (v one) (v two)))
         (a (vec2- (a one) (a two)))
         (seg-x (segment-collision-time
                 (vec2+ (car aabb-one) (x (presence one)))
                 (vec2+ (car aabb-two) (x (presence two)))
                 (vec-x v) (vec-x a)))
         seg-y)
    (unless (or (and (numberp (vec-b seg-x)) (not (minusp (vec-b seg-x))))
                (vec-a seg-x))
      (return-from aabb-collision-time seg-x))
    (setf seg-y (segment-collision-time
                 (vec2+ (cdr aabb-one) (y (presence one)))
                 (vec2+ (cdr aabb-two) (y (presence two)))
                 (vec-y v) (vec-y a)))
    (collision-overlap seg-x seg-y)))

(defmethod collision-update ((a matter) (b matter) c-time)
  ())

(defmethod collision-update :after ((matter matter) (other matter) c-time)
  (send-event matter `(:matter-collision :a ,matter :b ,other :c-time ,c-time)))