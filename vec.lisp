; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass vec ()
  ((a :initform nil
      :initarg :a
      :accessor x
      :accessor a)
   (b :initform nil
      :initarg :b
      :accessor y
      :accessor b)))

(set-dispatch-macro-character #\# #\v
  #'(lambda (stream subchar args)
      (declare (ignore subchar args))
      (destructuring-bind (a b) (read stream)
        (make-instance 'vec :a a :b b))))

(defmethod make-load-form ((vec vec) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of vec)
                  :a ,(slot-value vec 'a)
                  :b ,(slot-value vec 'b)))

(defun vec (a b)
  (make-instance 'vec :a a :b b))

(internal define-vec-op)
(defmacro define-vec-op (op)
  (let ((two-vec-op (intern (format nil "TWO-VEC~a" op)))
        (vec-op (intern (format nil "VEC~a" op))))
    `(progn
       (internal ,two-vec-op)
       (defmethod ,two-vec-op ((a vec) (b vec))
         (make-instance 'vec
                        :a (,op (x a) (x b))
                        :b (,op (y a) (y b))))
       
       
       (defmethod ,two-vec-op ((a vec) (b number))
         (make-instance 'vec
                        :a (,op (x a) b)
                        :b (,op (y a) b)))
       
       (defmethod ,two-vec-op ((a number) (b vec))
         (make-instance 'vec
                        :a (,op a (x b))
                        :b (,op a (y b))))

       (defmethod ,two-vec-op ((a number) (b number))
         (,op a b))
       
       (defun ,vec-op (&rest args)
         (reduce #',two-vec-op args)))))

(internal define-vec-ops)
(defmacro define-vec-ops (&rest ops)
  `(progn ,@(loop for op in ops
                  collect `(define-vec-op ,op))))

(define-vec-ops + - * /)

(defmacro define-vec-predicate (name (a b) &body predicate)
  `(defmethod ,name ((,a vec) &rest vecs)
     (loop for ,b in vecs
        unless ,@predicate return nil
        finally (return t))))

(define-vec-predicate vec= (a b)
  (and (= (a a) (a b))
       (= (b a) (b b))))

(define-vec-predicate vec< (a b)
  (< (b a) (a b)))

(define-vec-predicate vec<= (a b)
  (<= (b a) (a b)))

(define-vec-predicate vec> (a b)
  (> (a a) (b b)))

(define-vec-predicate vec>= (a b)
  (>= (a a) (b b)))

(defmethod overlap ((a vec) (b vec))
  (cond ((<= (a a) (a b) (b b) (b a)) (- (b b) (a b)))
        ((<= (a b) (a a) (b a) (b b)) (- (b a) (a a)))
        ((< (a a) (a b)) (- (b a) (a b)))
        ((< (a b) (a a)) (- (b b) (a a)))))

(defmethod copy-vec ((vec vec))
  (vec (a vec) (b vec)))

(defmethod min-max ((a vec) &rest vecs)
  (loop with result = (copy-vec a)
        for b in vecs
        when (< (a b) (a result)) do (setf (a result) (a b))
        when (> (b b) (b result)) do (setf (b result) (b b))
        finally (return result)))

(defmethod min-max ((a number) &rest numbers)
  (loop with result = (make-instance 'vec :a a :b a)
        for b in numbers
        when (< b (a result)) do (setf (a result) b)
        when (> b (b result)) do (setf (b result) b)
        finally (return result)))

(defmethod dot ((a vec) (b vec))
  (+ (* (x a) (x b))
     (* (y a) (y b))))

(defmethod magnitude ((vec vec))
  (with-accessors ((x x) (y y)) vec
    (sqrt (+ (* x x) (* y y)))))

(defmethod normalise ((vec vec))
  (let ((length (magnitude vec)))
    (if (zerop length) vec (vec/ vec length))))

(defmethod vec-vector ((vec vec))
  (let ((result (make-array 3 :element-type 'vec :fill-pointer 0)))
    (vector-push (a vec) result)
    (vector-push (b vec) result)
    result))

(defmethod print-object ((object vec) stream)
  (format stream "#v(~a~@{~@[ ~a~]~})" (a object) (b object)))