; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass collision-cell-test (test-case)
  ())

(defmethod set-up ((test collision-cell-test))
  (declare (ignore test))
  (reset-test-cells))