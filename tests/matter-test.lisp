; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass matter-test (test-case)
  ())

(defmethod set-up ((test matter-test))
  (reset-test-matter))

(def-test-method test-motion ((test matter-test))
  (assert-vec-equal #(0.0 0.0) (s *matter-a*))
  (update-motion *matter-a* 2.0)
  (assert-vec-equal (vec2* *gravity* 4.0) (s *matter-a*))
  (assert-vec-equal (vec2* *gravity* 2.0) (v *matter-a*))
  (setf (a *matter-b*) (vec2* -1 *gravity*))
  (update-motion *matter-b* 10.0)
  (assert-vec-equal #(0.0 0.0) (s *matter-b*))
  (update-motion *matter-c* 10.0)
  (assert-vec-equal #(0.0 0.0) (s *matter-c*)))

(def-test-method test-aabb-collision-time ((test matter-test))
  (assert-vec-equal #(t nil) (aabb-collision-time *matter-a* *matter-b*) "A")
  (setf (x (presence *matter-a*)) 200
        (y (presence *matter-a*)) 300
        (v *matter-a*) #(-2 -3))
  (assert-vec-equal #(85.0 100.0) (aabb-collision-time *matter-a* *matter-b*)
                    "B")
  (setf (v *matter-a*) #(-2 0))
  (assert-vec-equal #(nil t) (aabb-collision-time *matter-a* *matter-b*) "C")
  (setf (v *matter-a*) #(0 3))
  (assert-vec-equal #(nil t) (aabb-collision-time *matter-a* *matter-b*) "D"))
