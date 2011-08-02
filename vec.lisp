; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(internal define-vec-accessor)
(defmacro define-vec-accessor (postfix index)
  (let ((name (intern (format nil "VEC-~a" postfix))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (vec) (aref vec ,index))
       (defsetf ,name (vec) (value)
         `(setf (aref ,vec ,,index) ,value)))))

(define-vec-accessor a 0)
(define-vec-accessor x 0)
(define-vec-accessor b 1)
(define-vec-accessor y 1)

(internal define-vec2-sign)
(defmacro define-vec2-sign (sign)
  (let ((name (intern (format nil "VEC2~a" sign))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (&rest args)
         (loop 
            for arg in args
            for sum = arg
              then (etypecase sum
                     (number (etypecase arg
                               (number (,sign sum arg))
                               (vector (vector (,sign (vec-x arg) sum)
                                               (,sign (vec-y arg) sum)))))
                     (vector (etypecase arg
                               (number (vector (,sign (vec-x sum) arg)
                                               (,sign (vec-y sum) arg)))
                               (vector (vector (,sign (vec-x sum) (vec-x arg))
                                               (,sign (vec-y sum)
                                                    (vec-y arg)))))))
            finally (return sum))))))

(define-vec2-sign +)
(define-vec2-sign -)
(define-vec2-sign *)
(define-vec2-sign /)

(internal define-vec-predicate)
(defmacro define-vec-predicate (name (a b) &body predicate)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,a &rest args)
       (loop for ,b in args
             unless ,@predicate return nil
             finally (return t)))))

(define-vec-predicate vec2= (one two)
  (and (= (vec-a one) (vec-a two))
       (= (vec-b one) (vec-b two))))

(define-vec-predicate vec2-eql (one two)
  (and (eql (vec-a one) (vec-a two))
       (eql (vec-b one) (vec-b two))))

(internal define-vec2-comparisons)
(defmacro define-vec2-comparisons (part)
  `(progn
     ,@(loop with accessor = (intern (format nil "VEC-~a" part))
             for sign in '(< <= > >=)
             for predicate-name = (intern (format nil "VEC2~a~a" part sign))
             collect `(define-vec-predicate ,predicate-name (one two)
                        (,sign (,accessor one) (,accessor two))))))

(define-vec2-comparisons a)
(define-vec2-comparisons b)
(define-vec2-comparisons x)
(define-vec2-comparisons y)

(define-vec-predicate axis2= (one two)
  (or (vec2= one two)
      (vec2= (vec2* -1 one) two)))

(declaim (inline overlap-vec2))
(defun overlap-vec2 (one two)
  (vector (max (vec-a one) (vec-a two))
          (min (vec-b one) (vec-b two))))

(declaim (inline vec2-overlap-p))
(defun vec2-overlap-p (one two)
  (<= (max (vec-a one) (vec-a two))
      (min (vec-b one) (vec-b two))))

(declaim (inline dot2))
(defun dot2 (one two)
  (+ (* (vec-a one) (vec-a two))
     (* (vec-b one) (vec-b two))))

(declaim (inline seg-length))
(defun seg-length (seg)
  (- (vec-b seg) (vec-a seg)))

(declaim (inline magnitude))
(defun magnitude (vec)
  (sqrt (+ (expt (vec-a vec) 2)
           (expt (vec-b vec) 2))))

(declaim (inline normalise))
(defun normalise (vec)
  (let ((length (magnitude vec)))
    (if (zerop length) vec (vec2/ vec length))))

(declaim (inline vec2-px-to-m vec2-m-to-px))
(defun vec2-px-to-m (px) (vec2/ px *ppm*))
(defun vec2-m-to-px (m) (vec2* m *ppm*))
