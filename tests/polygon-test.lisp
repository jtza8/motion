; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass polygon-test (test-case)
  ())

(def-test-method test-projection ((test polygon-test))
  (let ((polygon (make-instance 'polygon 
                                :points (list #v(10 22)
                                              #v(54 43)
                                              #v(63 32)
                                              #v(12 9)))))
    (assert-true (vec= #v(10 63) (project polygon #v(1 0))))
    (assert-true (vec= #v(9 43) (project polygon #v(0 1))))))