; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass vec-test (test-case)
  ())

(def-test-method test-vec2+ ((test vec-test))
  (assert-equal 3 (vec2+ 1 2))
  (assert-true (equalp #(4 5) (vec2+ 3 #(1 2))))
  (assert-true (equalp #(4 7) (vec2+ #(3 5) #(1 2))))
  (assert-true (equalp #(4 5) (vec2+ #(1 2) 3)))
  (assert-condition 'error (vec2+ 1 nil)))

(def-test-method test-vec2= ((test vec-test))
  (assert-true (vec2= #(1 2) #(1 2)))
  (assert-false (vec2= #(2 2) #(1 2)))
  (assert-false (vec2= #(1 1) #(1 2)))
  (assert-true (vec2= #(1.0 2.0) #(1 2))))

(def-test-method test-vec2-comparison ((test vec-test))
  (assert-true (vec2a< #(1 5) #(2 3)))
  (assert-true (vec2a<= #(1 5) #(2 3)))
  (assert-true (vec2a<= #(1 5) #(1 3)))
  (assert-true (vec2a> #(2 3) #(1 5)))
  (assert-true (vec2a>= #(2 3) #(1 5)))
  (assert-true (vec2a<= #(1 5) #(1 3))))

(def-test-method test-axis2= ((test vec-test))
  (assert-true (axis2= #(1 2) #(1 2)))
  (assert-true (axis2= #(-1 -2) #(1 2))))

(def-test-method test-overlap-vec2 ((test vec-test))
  (assert-true (vec2= #(4 5) (overlap-vec2 #(2 5) #(4 6))))
  (assert-true (vec2= #(4 5) (overlap-vec2 #(4 6) #(2 5))))
  (assert-true (vec2= #(3 4) (overlap-vec2 #(2 5) #(3 4))))
  ; Negative overlap:
  (assert-true (vec2= #(8 5) (overlap-vec2 #(2 5) #(8 12)))))

(def-test-method test-overlap-vec2-p ((test vec-test))
  (assert-true (vec2-overlap-p #(1 3) #(2 4)))
  (assert-true (vec2-overlap-p #(1 2) #(2 3)))
  (assert-false (vec2-overlap-p #(1 3) #(4 6))))

(def-test-method test-dot2 ((test vec-test))
  (assert-equal 23 (dot2 #(2 3) #(4 5))))

(def-test-method test-seg-length ((test vec-test))
  (assert-equal 7 (seg-length #(3 10)))
  (assert-equal -7 (seg-length #(10 3))))

(def-test-method test-magnitude ((test vec-test))
  (assert-equal 5.0 (magnitude #(3 4))))

(def-test-method test-normalise ((test vec-test))
  (assert-true (vec2= #(0.6 0.8) (normalise #(3 4))))
  (assert-true (vec2= #(0 0) (normalise #(0 0)))))