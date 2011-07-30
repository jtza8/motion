; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(declaim (inline point-collision-time))
(defun point-collision-time (s v a)
  (if (= a 0.0)
      (if (= v 0.0) 
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

(declaim (inline collision-possible-p))
(defun collision-possible-p (collision)
  (cond ((eq (vec-a collision) t) t)
        ((null (vec-a collision)) nil)
        ((> (vec-a collision) (vec-b collision)) nil)
        ((minusp (vec-b collision)) nil)
        (t t)))

(declaim (inline collision<))
(defun collision< (one two)
  (cond ((and (eq (vec-a one) (vec-a two))) nil)
        ((null (vec-a one)) nil)
        ((null (vec-a two)) t)
        ((eq (vec-a one) t)
         (and (numberp (vec-a two))
              (plusp (vec-a two))))
        ((eq (vec-a two) t)
         (and (numberp (vec-a one))
              (minusp (vec-a one))))
        (t (vec2a< one two))))