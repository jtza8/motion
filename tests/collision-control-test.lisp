; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-control-test (test-case)
  ())

(defmethod set-up ((test collision-control-test))
  (declare (ignore test))
  (reset-test-polys))

