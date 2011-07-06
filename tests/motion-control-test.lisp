; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass motion-control-test (test-case)
  ())

;; (def-test-method test-cell-at ((test motion-control-test))
;;   (let ((control (make-instance 'motion-control
;;                                 :rows 4 :columns 5
;;                                 :polygons `(,*poly-a* ,*poly-b*)
;;                                 :cell-width)