; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass matter-test (test-case)
  ())

(defmethod set-up ((test matter-test))
  (reset-test-matter))

(def-test-method test-motion ((test matter-test))
  (assert-vec-equal #v(0.0 0.0) (s *matter-a*))
  (update-motion *matter-a* 2.0)
  (assert-vec-equal (vec* *gravity* 4.0) (s *matter-a*))
  (assert-vec-equal (vec* *gravity* 2.0) (v *matter-a*))
  (setf (a *matter-b*) (vec* -1 *gravity*))
  (update-motion *matter-b* 10.0)
  (assert-vec-equal #v(0.0 0.0) (s *matter-b*))
  (update-motion *matter-c* 10.0)
  (assert-vec-equal #v(0.0 0.0) (s *matter-c*)))
