; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)
(use-package :xlunit)

(defvar *poly-a*)
(defvar *poly-b*)
(defvar *poly-c*)

(defvar *matter-a*)
(defvar *matter-b*)
(defvar *matter-c*)

(defvar *cell-a*)

(defun reset-test-polys ()
  (setf *poly-a* 
        (make-instance 'poly
                       :points '(#(10 0) #(65 25) #(15 35) #(0 20))
                       :x 15 :y 25)
        *poly-b*
        (make-instance 'poly
                       :points '(#(5 0) #(30 35) #(0 25))
                       :x 65 :y 10)
        *poly-c*
        (make-instance 'poly
                       :points '(#(2 2) #(510 2) #(510 126) #(2 126))
                       :x 5 :y 467)))

(defun reset-test-matter ()
  (reset-test-polys)
  (setf *matter-a* (make-instance 'matter :presence *poly-a*)
        *matter-b* (make-instance 'matter :presence *poly-b*)
        *matter-c* (make-instance 'matter :presence *poly-c*)))

(defun reset-test-cells ()
  (reset-test-matter)
  (setf *cell-a* (make-instance 'collision-cell
                 :matters (list *matter-a* *matter-b* *matter-c*))))