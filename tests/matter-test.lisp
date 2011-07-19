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

(def-test-method test-point-collision-time ((test matter-test))
  (assert-equal 1.1735992 (point-collision-time 10 5 6))
  (assert-equal 2.0 (point-collision-time 10 5 0))
  (assert-equal 1.8257419 (point-collision-time 10 0 6))
  (assert-equal 0.0 (point-collision-time 0 5 6))
  (assert-equal nil (point-collision-time 10 0 0))
  (assert-equal 1.8257419 (point-collision-time 10 0 6)))

(def-test-method test-segment-collision-time ((test matter-test))
  (assert-vec-equal #v(0.123105526 0.47213602)
                    (segment-collision-time #v(1 2) #v(3 5) 8 2)))

(def-test-method test-aabb-collision-time ((test matter-test))
  (setf (v *matter-a*) #v(-10.0 -10.0)
        (v *matter-b*) #v(+10.0 +10.0))
  (assert-vec-equal #v(15 25) (abs-point *poly-a* (car (aabb *poly-a*))))
  (assert-vec-equal #v(80 60) (abs-point *poly-a* (cdr (aabb *poly-a*))))
  (assert-vec-equal #v(-1.0 0.75) (aabb-collision-time *matter-a* *matter-b*)))