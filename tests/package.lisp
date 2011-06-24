; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)
(use-package :xlunit)

(defvar *poly-a*)
(defvar *poly-b*)

(defun reset-test-polys ()
  (setf *poly-a* 
        (make-instance 'polygon
                       :points '(#v(10 0) #v(65 25) #v(15 35) #v(0 20))
                       :x 15 :y 25)
        *poly-b*
        (make-instance 'polygon
                       :points '(#v(5 0) #v(30 35) #v(0 25))
                       :x 65 :y 10)))

