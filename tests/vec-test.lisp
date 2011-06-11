; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass vec-test (test-case)
  ())

(def-test-method test-reader-macro ((test vec-test))
  (let ((vec #v(1 2)))
    (assert-equal 1 (x vec))
    (assert-equal 2 (y vec))))

(def-test-method test-vec-ops ((test vec-test))
  (let ((vec (vec+ #v(1 2) #v(2 3) #v(4 5))))
    (assert-equal 7 (x vec))
    (assert-equal 10 (y vec)))
  (let ((vec (vec+ #v(1 2) 3)))
    (assert-equal 4 (x vec))
    (assert-equal 5 (y vec)))
  (let ((vec (vec+ 5 #v(3 4))))
    (assert-equal 8 (x vec))
    (assert-equal 9 (y vec)))
  (assert-equal 12 (vec+ 4 8))
  (let ((vec (vec- 5 #v(3 4))))
    (assert-equal 2 (x vec))
    (assert-equal 1 (y vec)))
  (let ((vec (vec/ 12 #v(3 4))))
    (assert-equal 4 (x vec))
    (assert-equal 3 (y vec))))

(def-test-method test-compare ((test vec-test))
  (let ((a #v(0 3))
        (b #v(4 6))
        (c #v(3 5))
        (d #v(2 6)))
    (assert-true (vec= #v(1 2) #v(1 2) #v(1 2)))
    (assert-false (vec= #v(1 2) #v(1 2) #v(1 3)))
    (assert-true (vec< a b))
    (assert-false (vec< b a))
    (assert-false (vec< a c))
    (assert-true (vec<= a c))
    (assert-true (vec> b a))
    (assert-false (vec> a b))
    (assert-false (vec> c a))
    (assert-true (vec>= c a))
    (assert-true (vec-overlap-p a d))
    (assert-true (vec-overlap-p d c))
    (assert-false (vec-overlap-p a b))
    (assert-false (vec-overlap-p a b c d))))

(def-test-method test-min-max ((test vec-test))
  (let ((a #v(1 2)))
    (assert-true (vec= #v(-3 8) (min-max #v(-3 2) #v(1 8))))
    (assert-true (vec= #v(-3 8) (min-max a #v(2 4) #v(-3 8))))
    (assert-true (vec= #v(1 2) a))
    (assert-true (vec= #v(-3 6) (min-max 2 3 -2 -3 6 5)))))

(def-test-method test-unit-vec ((test vec-test))
  (let ((a (unit-vec #v(-1 -2) #v(1 0))) ; 45 degrees.
        (b (unit-vec #v(-1 1) #v(1 -1)))) ; -45 degrees.
    (assert-equal (x a) (y a))
    (assert-equal (- (x b)) (y b))))