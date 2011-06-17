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
        (make-instance 'polygon :x 100 :y 100
                       :points '(#v(2 2) #v(10 50) #v(60 50) #v(50 2)))))

(defmacro assert-equality (test expected actual)
  (let ((expected-var (gensym))
        (actual-var (gensym)))
    `(let ((,expected-var ,expected)
           (,actual-var ,actual))
       (assert-true (,test ,expected-var ,actual-var)
                    (format nil "Expected ~a but got ~a."
                            ,expected-var ,actual-var)))))

(def-test-method test-projection ((test polygon-test))
  (assert-equality vec= #v(10 63) (project *test-polygon-a* #v(1 0)))
  (assert-equality vec= #v(9 43) (project *test-polygon-a* #v(0 1)))
  (assert-equality vec= #v(14.847 68.579)
                   (project *test-polygon-a* #v(0.707 0.707)))
  (setf (x *test-polygon-a*) 100)
  (assert-equality vec= #v(110 163) (project *test-polygon-a* #v(1 0))))

(def-test-method test-calc-axes ((test polygon-test))
  (let ((axes '(#v(-0.9883717 -0.15205719) #v(0.41110775 -0.91158676)
                #v(0.7739573 0.6332378) #v(-0.4307296 0.9024811))))
    (loop for a in axes
          for b in (axes *test-polygon-a*)
          do (assert-equality vec= a b))))

(def-test-method test-collides-p ((test polygon-test))
  (setf (x *test-polygon-a*) 0
        (y *test-polygon-a*) 0
        (x *test-polygon-b*) 0
        (y *test-polygon-b*) 0)
  (assert-true (collides-p *test-polygon-a* *test-polygon-b*))
  (collides-p *test-polygon-a* *test-polygon-b*))

(defun test-collision-manually ()
  (click:with-display-system (click:screen-colour '(1.0 1.0 1.0 1.0))
    (setf *polygons* `(,*test-polygon-a* ,*test-polygon-b*))
    (let* ((a (make-polygon-igo
                :polygon *test-polygon-a*
                :width 64 :height 64))
           (b (make-polygon-igo
                :polygon *test-polygon-b*
                :width 64 :height 64)))
      (click:add-to-root a)
      (click:add-to-root b))))