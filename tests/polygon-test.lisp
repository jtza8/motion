; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defvar *test-polygon-a*)
(defvar *test-polygon-b*)

(defclass polygon-test (test-case)
  ())

(defmethod set-up ((test polygon-test))
  (setf *test-polygon-a*
        (make-instance 'polygon
                       :points '(#v(10 22) #v(54 43) #v(63 32) #v(12 9))))
  (setf *test-polygon-b*
        (make-instance 'polygon
                       :points '(#v(0 0) #v(10 50) #v(60 50) #v(50 0)))))

(def-test-method test-projection ((test polygon-test))
  (assert-true (vec= #v(10 63) (project *test-polygon-a* #v(1 0))))
  (assert-true (vec= #v(9 43) (project *test-polygon-a* #v(0 1))))
  (setf (x *test-polygon-a*) 100)
  (assert-true (vec= #v(110 163) (project *test-polygon-a* #v(1 0)))))

(def-test-method test-calc-axes ((test polygon-test))
  (let ((axes '(#v(-0.15205719 0.9883717) #v(-0.91158676 -0.41110775)
                #v(0.6332378 -0.7739573)  #v(0.9024811 0.4307296))))
    (loop for a in axes
          for b in (axes *test-polygon-a*)
          do (assert-true (vec= a b)))))

(def-test-method test-collides-p ((test polygon-test))
  (assert-true (collides-p *test-polygon-a* *test-polygon-b*))
  (setf (x *test-polygon-a*) 100)
  (collides-p *test-polygon-a* *test-polygon-b*))

;; (defun test-collision-manually ()
;;   (click:with-display-system ()
;;     (let ((a (click:make-polygon-sprite 