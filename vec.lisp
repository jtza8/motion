; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass vec ()
  ((x :initform 0
      :initarg :x
      :accessor x
      :accessor a)
   (y :initform 0
      :initarg :y
      :accessor y
      :accessor b)))

(set-dispatch-macro-character #\# #\v
  #'(lambda (stream subchar args)
      (declare (ignore subchar args))
      (destructuring-bind (x y) (read stream)
        `(make-instance 'vec :x ,x :y ,y))))

(macrolet ((make-vec-op (op)
             (let ((two-vec-op (intern (format nil "TWO-VEC~a" op)))
                   (vec-op (intern (format nil "VEC~a" op))))
               `(progn
                  (internal ,two-vec-op)
                  (defmethod ,two-vec-op ((a vec) (b vec))
                    (make-instance 'vec
                                   :x (,op (x a) (x b))
                                   :y (,op (y a) (y b))))
                  
                  (defmethod ,two-vec-op ((a vec) (b number))
                    (make-instance 'vec
                                   :x (,op (x a) b)
                                   :y (,op (y a) b)))
                  
                  (defmethod ,two-vec-op ((a number) (b vec))
                    (make-instance 'vec
                                   :x (,op a (x b))
                                   :y (,op a (y b))))

                  (defmethod ,two-vec-op ((a number) (b number))
                    (,op a b))
                  
                  (defun ,vec-op (&rest args)
                    (reduce #',two-vec-op args))))))
  (make-vec-op +)
  (make-vec-op -)
  (make-vec-op *)
  (make-vec-op /))

(defmacro define-vec-predicate (name (a b) &body predicate)
  `(defmethod ,name ((,a vec) &rest vecs)
     (loop for ,b in vecs
        unless ,@predicate return nil
        finally (return t))))

(define-vec-predicate vec-overlap-p (a b)
  (or (<= (a a) (a b) (b a))
      (<= (a a) (b b) (b a))))
  
(define-vec-predicate vec< (a b)
  (< (b a) (a b)))

(define-vec-predicate vec<= (a b)
  (<= (b a) (a b)))

(define-vec-predicate vec> (a b)
  (> (a a) (b b)))

(define-vec-predicate vec>= (a b)
  (>= (a a) (b b)))

(defmethod copy-vec ((vec vec))
  (make-instance 'vec :x (x vec) :y (y vec)))

(defmethod min-max ((a vec) &rest vecs)
  (loop with result = (copy-vec a)
        for b in vecs
        when (< (a b) (a result)) do (setf (a result) (a b))
        when (> (b b) (b result)) do (setf (b result) (b b))
        finally (return result)))

(defmethod min-max ((a number) &rest numbers)
  (loop with result = (make-instance 'vec :x a :y a)
        for b in numbers
        when (< b (a result)) do (setf (a result) b)
        when (> b (b result)) do (setf (b result) b)
        finally (return result)))

(defmethod dot-product ((a vec) (b vec))
  (+ (* (x a) (x b))
     (* (y a) (y b))))

(defmethod unit-vec ((a vec) (b vec))
  (let* ((delta (vec- b a))
         (length (sqrt (+ (expt (x delta) 2) (expt (y delta) 2)))))
    (make-instance 'vec
                   :x (/ (x delta) length)
                   :y (/ (y delta) length))))

(defmethod print-object ((object vec) stream)
  (format stream "#<~a :X ~a :Y ~a>" (type-of object) (x object) (y object)))