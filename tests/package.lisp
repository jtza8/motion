; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)
(use-package :xlunit)

(defvar *poly-a*)
(defvar *poly-b*)
(defvar *poly-c*)

(defun reset-test-polys ()
  (setf *poly-a* 
        (make-instance 'poly
                       :points '(#v(10 0) #v(65 25) #v(15 35) #v(0 20))
                       :x 15 :y 25)
        *poly-b*
        (make-instance 'poly
                       :points '(#v(5 0) #v(30 35) #v(0 25))
                       :x 65 :y 10)
        *poly-c*
        (make-instance 'poly
                       :points '(#v(2 2) #v(510 2) #v(510 126) #v(2 126))
                       :x 5 :y 467)))

