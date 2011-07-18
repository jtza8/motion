; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell (listenable listener)
  ((objects :initform '()
            :initarg :objects
            :accessor objects)
   (expired-objects :initform '())
   (collisions :initform '())))

(internal cons-hash)
(defun cons-hash (car cdr)
  (min (sxhash (cons car cdr))
       (sxhash (cons cdr car))))

(defmethod collision-record ((cell collision-cell) (a matter) (b matter))
  (with-slots (collisions) cell
    (let ((data (cdr (assoc (cons-hash a b) collisions :test #'eql))))
      (when data (aref data 0)))))

(defmethod (setf collision-record) (time (cell collision-cell)
                                    (a matter) (b matter))
  (with-slots (collisions) cell
    (let ((item (assoc (cons-hash a b) collisions :test #'eql))
          (data (vector time a b (s a) (s b) (v a) (v b) (a a) (a b))))
      (if (null item)
          (push (cons (cons-hash a b) data) collisions)
          (setf (cdr item) data))
      time)))

(defun row-contains-matter-p (row matter)
  (or (eq (aref (cdr row) 1) matter)
      (eq (aref (cdr row) 2) matter)))

(defmethod clear-collisions ((cell collision-cell) &optional matter)
  (with-slots (collisions) cell
    (setf collisions
          (if matter
              (loop for subset on collisions
                    while (cadr subset)
                    when (row-contains-matter-p (cadr subset) matter)
                      do (setf (cdr subset) (cddr subset))
                    finally
                      (return
                        (if (row-contains-matter-p (car collisions) matter)
                            (cdr collisions)
                            collisions)))
              '()))))

(defmethod detect-collisions ((cell collision-cell))
  (with-slots (objects) cell
    (loop for subset on objects
          do (loop with a = (car subset) and delta and axis
                   for b in (cdr subset)
                   do (setf (values delta axis)
                            (overlap (presence a) (presence b)))
                   unless (= delta 0.0)
                     do (collision-update a b delta axis)))))