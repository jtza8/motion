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
      :accessor b)
   (c :initform nil
      :initarg :c
      :accessor z
      :accessor c)))

(set-dispatch-macro-character #\# #\v
  #'(lambda (stream subchar args)
      (declare (ignore subchar args))
      (destructuring-bind (a b &optional c) (read stream)
        (make-instance 'vec :a a :b b :c c))))

(defmethod make-load-form ((vec vec) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of vec)
                  :a ,(slot-value vec 'a)
                  :b ,(slot-value vec 'b)
                  :c ,(slot-value vec 'c)))

(defun vec (a b &optional c)
  (make-instance 'vec :a a :b b :c c))

(internal define-vec-op)
(defmacro define-vec-op (op)
  (let ((two-vec-op (intern (format nil "TWO-VEC~a" op)))
        (vec-op (intern (format nil "VEC~a" op))))
    `(progn
       (internal ,two-vec-op)
       (defmethod ,two-vec-op ((a vec) (b vec))
         (make-instance 'vec
                        :a (,op (x a) (x b))
                        :b (,op (y a) (y b))
                        :c (progn
                             (when (= (check-vec-dimensions a b) 3)
                               (,op (z a) (z b))))))
       
       
       (defmethod ,two-vec-op ((a vec) (b number))
         (make-instance 'vec
                        :a (,op (x a) b)
                        :b (,op (y a) b)
                        :c (unless (null (z a))
                             (* (z a) b))))
       
       (defmethod ,two-vec-op ((a number) (b vec))
         (make-instance 'vec
                        :a (,op a (x b))
                        :b (,op a (y b))
                        :c (unless (null (z b))
                             (* (z b) a))))

       (defmethod ,two-vec-op ((a number) (b number))
         (,op a b))
       
       (defun ,vec-op (&rest args)
         (reduce #',two-vec-op args)))))

(internal define-vec-ops)
(defmacro define-vec-ops (&rest ops)
  `(progn ,@(loop for op in ops
                  collect `(define-vec-op ,op))))

(define-vec-ops + - * /)

(defmacro define-vec-predicate (name (a b &optional c) &body predicate)
  `(defmethod ,name ((,a vec) &rest vecs)
     ,(when (null c) `(assert-vecs-2d ,a))
     (loop for ,b in vecs
        ,@(when (null c) `(do (assert-vecs-2d ,b)))
        unless ,@predicate return nil
        finally (return t))))

(define-vec-predicate overlaps-p (a b)
  (or (<= (a a) (a b) (b a))
      (<= (a a) (b b) (b a))))

(define-vec-predicate vec= (a b c)
  (and (= (a a) (a b))
       (= (b a) (b b))
       (or (or (null (c a)) (null (c b)))
           (= (c a) (c b)))))

(define-vec-predicate vec< (a b)
  (< (b a) (a b)))

(define-vec-predicate vec<= (a b)
  (<= (b a) (a b)))

(define-vec-predicate vec> (a b)
  (> (a a) (b b)))

(define-vec-predicate vec>= (a b)
  (>= (a a) (b b)))

(defmethod copy-vec ((vec vec))
  (vec (a vec) (b vec) (c vec)))

(defmethod min-max ((a vec) &rest vecs)
  (apply #'assert-vecs-2d a vecs)
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
     (* (y a) (y b))
     (if (= (check-vec-dimensions a b) 3)
         (* (z a) (z b))
         0)))

(defmethod unit-vec ((a vec) (b vec))
  (assert-vecs-2d a b)
  (let* ((delta (vec- b a))
         (length (sqrt (+ (expt (x delta) 2) (expt (y delta) 2)))))
    (vec (/ (x delta) length) (/ (y delta) length))))

(defmethod unit-vec= ((a vec) &rest b)
  (apply #'vec=
         (mapcar (lambda (v) (if (and (< (x v) 0) (< (y v) 0)) (vec* -1 v) v))
                 (cons a b))))


(defmethod print-object ((object vec) stream)
  (format stream "#v(~a~@{~@[ ~a~]~})" (a object) (b object) (c object)))