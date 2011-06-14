; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *test-polygon-a*
  (make-instance 'polygon 
                 :points (list #v(10 22)
                               #v(54 43)
                               #v(63 32)
                               #v(12 9))))

(defclass polygon-test (test-case)
  ())

(def-test-method test-projection ((test polygon-test))
  (assert-true (vec= #v(10 63) (project *test-polygon-a* #v(1 0))))
  (assert-true (vec= #v(9 43) (project *test-polygon-a* #v(0 1)))))

(def-test-method test-calc-axes ((test polygon-test))
  (let ((axes '(#v(0.15205719 -0.9883717) #v(0.9024811 0.4307296)
                #v(0.6332378 -0.7739573)  #v(-0.91158676 -0.41110775))))
    (loop for a in axes
          for b in (calc-axes *test-polygon-a*)
          do (assert-true (vec= a b)))))

(def-test-method test-calc-projections ((test polygon-test))
  (let ((projections 
         '((#v(0.15205719 -0.9883717)   . #v(-34.288895 -7.0706587))
           (#v(0.9024811 0.4307296)     . #v(14.706339 70.639656))
           (#v(0.6332378 -0.7739573)    . #v(-10.694683 15.127348))
           (#v(-0.91158676 -0.41110775) . #v(-70.58541 -14.639011)))))
    (loop for a in projections
          for b in (calc-projections *test-polygon-a*)
          do (progn
               (assert-true (vec= (car a) (car b)))
               (assert-true (vec= (cdr a) (cdr b)))))))