; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell-test (test-case)
  ())

(defmethod set-up ((test collision-cell-test))
  (declare (ignore test))
  (reset-test-polys))

(def-test-method test-collision-record ((test collision-cell-test))
  (let ((cell (make-instance 'collision-cell 
                :objects `(,*matter-a* ,*matter-b*))))
    (assert-true (numberp (cons-hash 2 3)))
    (assert-equal (cons-hash 1 2) (cons-hash 2 1))
    (assert-false (collision-record cell *matter-a* *matter-b*))
    (setf (collision-record cell *matter-a* *matter-b*) #v(1.23 3.42))
    (assert-vec-equal (collision-record cell *matter-a* *matter-b*)
                      #v(1.23 3.42))
    (setf (collision-record cell *matter-b* *matter-a*) #v(1.32 2.12))
    (assert-vec-equal (collision-record cell *matter-b* *matter-a*)
                      #v(1.32 2.12))
    (assert-vec-equal (collision-record cell *matter-a* *matter-b*)
                      #v(1.32 2.12))
    (with-slots (collisions) cell
      (assert-true (row-contains-matter-p (car collisions) *matter-a*))
      (assert-false (row-contains-matter-p (car collisions) *matter-c*))
      (assert-equal 1 (length collisions))
      (clear-collisions cell *matter-c*)
      (assert-equal 1 (length collisions))
      (setf (collision-record cell *matter-b* *matter-c*) #v(3.22 1.52))
      (assert-equal 2 (length collisions))
      (clear-collisions cell *matter-a*)
      (assert-equal 1 (length collisions))
      (setf (collision-record cell *matter-b* *matter-a*) #v(1.32 2.12))
      (assert-equal 2 (length collisions))
      (clear-collisions cell *matter-a*)
      (assert-equal 1 (length collisions))
      (setf (collision-record cell *matter-b* *matter-a*) #v(1.32 2.12))
      (assert-equal 2 (length collisions))
      (clear-collisions cell)
      (assert-equal 0 (length collisions)))))

;; (def-test-method test-detect-collisions ((test collision-cell-test))
;;   (let ((cell (make-instance 'collision-cell
;;                                 :polys `(,*poly-a* ,*poly-b*)))
;;         (listener-a (make-instance 'dummy-listener
;;                                    :desired-events '(:collision)))
;;         (listener-b (make-instance 'dummy-listener
;;                                    :desired-events '(:collision))))
;;     (subscribe *poly-a* listener-a)
;;     (subscribe *poly-b* listener-b)
;;     (detect-collisions cell)
;;     (assert-equal nil (latest-event listener-a) "For listener A.")
;;     (assert-equal nil (latest-event listener-b) "For listener B.")
;;     (incf (y *poly-b*) 9)
;;     (detect-collisions cell)
;;     (assert-equal :collision (event-type (latest-event listener-a)))
;;     (assert-equal :collision (event-type (latest-event listener-b)))))