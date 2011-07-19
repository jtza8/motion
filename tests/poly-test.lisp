; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass poly-test (test-case)
  ())

(defmethod set-up ((test poly-test))
  (declare (ignore test))
  (reset-test-polys))

(def-test-method test-project ((test poly-test))
  (assert-true (vec= (project *poly-a* #v(-0.414 0.910))
                     #v(-4.16 25.64))))

(def-test-method test-update-axes ((test poly-test))
  (loop for a in '(#v(0.8944272 0.4472136) #v(0.70710677 -0.70710677)
                   #v(-0.19611613 -0.9805806) #v(-0.41380295 0.9103665))
        for b in (axes *poly-a*)
        do (assert-vec-equal a b)))

(def-test-method test-overlap ((test poly-test))
  (assert-equal 0.0 (overlap *poly-a* *poly-b*))
  (assert-equal 0.0 (overlap *poly-b* *poly-a*))
  (incf (y *poly-b*) 8.2)
  (assert-not-eql 0.0 (overlap *poly-a* *poly-b*))
  (assert-not-eql 0.0 (overlap *poly-b* *poly-a*))
  (setf (x *poly-a*) 17 (y *poly-a*) 20
        (x *poly-b*) 47 (y *poly-b*) 11)
  (assert-not-eql 0.0 (overlap *poly-b* *poly-a*))
  (setf (x *poly-a*) 134 (y *poly-a*) 88
        (x *poly-b*) 118 (y *poly-b*) 85)
  (assert-equal 4.417412 (overlap *poly-a* *poly-b*)))

(def-test-method test-aabb ((test poly-test))
  (let ((result (aabb *poly-a*)))
    (assert-vec-equal #v(0 0) (car result))
    (assert-vec-equal #v(65 35) (cdr result))))

(defun profile-ovelap ()
  (let* ((polys 
          (loop for i below 1000
             collect (make-instance 'poly
                       :points (points (if (= 0 (random 2))
                                           *poly-c*
                                           *poly-a*))
                       :x (random 1000)
                       :y (random 1000)))))
    (time
     (loop for part on polys
           do (loop with a = (car part)
                    for b in (cdr part)
                    do (overlap a b))))))
                   

(defun test-collision-manually ()
  (click:with-display-system (click:screen-colour '(1.0 1.0 1.0 1.0)
                              click:screen-width 800
                              click:screen-height 600)
    (let* ((a (make-poly-igo
                :poly *poly-a*
                :width 64 :height 64))
           (b (make-poly-igo
                :poly *poly-b*
                :width 32 :height 64))
           (c (make-poly-igo
                :poly *poly-c*
                :width 512 :height 128))
           (m-control (make-instance 'motion-control
                                     :objects `(,*matter-a* ,*matter-b*
                                                            ,*matter-c*))))
      (click:add-root-listener m-control)
      (click:add-to-root a)
      (click:add-to-root b)
      (click:add-to-root c))))
