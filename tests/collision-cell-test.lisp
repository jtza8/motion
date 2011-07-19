; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell-test (test-case)
  ())

(defmethod set-up ((test collision-cell-test))
  (declare (ignore test))
  (reset-test-cells))

(def-test-method test-matter-in-collision-row-p ((test collision-cell-test))
  (let ((row-a (make-collision-row :time 3.0 :a *matter-a* :b *matter-b*)))
    (assert-true (matter-in-collision-row-p *matter-a* row-a))
    (assert-true (matter-in-collision-row-p *matter-b* row-a))
    (assert-false (matter-in-collision-row-p *matter-c* row-a))))

(def-test-method test-query-collision ((test collision-cell-test))
  (assert-false (cr-time (query-collision *cell-a* *matter-a* *matter-b*)))
  (setf (v *matter-b*) #v(10.0 10.0))
  (assert-false (cr-time (query-collision *cell-a* *matter-a* *matter-b*)))
  (assert-vec-equal #v(-2.0 1.5)
                    (cr-time (query-collision *cell-a* *matter-a*
                                              *matter-b* t))))

(def-test-method test-add-matter ((test collision-cell-test))
  (let ((matter (make-instance 'matter :presence *poly-b*)))
    (add-matter *cell-a* matter)
    (assert-true (find matter (slot-value *cell-a* 'matters)))
    (assert-true (find matter (slot-value *cell-a* 'expired-matters)))))

(def-test-method test-update-expired-matters ((test collision-cell-test))
  (assert-equal 3 (length (slot-value *cell-a* 'collisions)))
  (assert-equal 0 (length (slot-value *cell-a* 'expired-matters))))

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