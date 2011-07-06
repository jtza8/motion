; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell-test (test-case)
  ())

(defmethod set-up ((test collision-cell-test))
  (declare (ignore test))
  (reset-test-polys))

(def-test-method test-detect-collisions ((test collision-cell-test))
  (let ((cell (make-instance 'collision-cell
                                :polygons `(,*poly-a* ,*poly-b*)))
        (listener-a (make-instance 'dummy-listener
                                   :desired-events '(:collision)))
        (listener-b (make-instance 'dummy-listener
                                   :desired-events '(:collision))))
    (subscribe *poly-a* listener-a)
    (subscribe *poly-b* listener-b)
    (detect-collisions cell)
    (assert-equal nil (latest-event listener-a) "For listener A.")
    (assert-equal nil (latest-event listener-b) "For listener B.")
    (incf (y *poly-b*) 9)
    (detect-collisions cell)
    (assert-equal :collision (event-type (latest-event listener-a)))
    (assert-equal :collision (event-type (latest-event listener-b)))))