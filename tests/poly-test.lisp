; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass poly-test (test-case)
  ())

(defun dump-to-string (object &optional name)
  (format nil "~%~a"
          (with-output-to-string (out)
            (unless (null name)
              (format out "For ~a:~%" name))
            (describe object out))))

(defmethod set-up ((test poly-test))
  (declare (ignore test))
  (reset-test-polys))

(def-test-method test-project ((test poly-test))
  (assert-true (vec= (project *poly-a* #v(-0.414 0.910))
                     #v(-4.16 25.64))))

(def-test-method test-collide ((test poly-test))
  (assert-vec-equal #v(0 0) (collide *poly-a* *poly-b*))
  (assert-vec-equal #v(0 0) (collide *poly-b* *poly-a*))
  (incf (y *poly-b*) 8.2)
  (assert-false (vec= #v(0 0) (collide *poly-a* *poly-b*)))
  (assert-false (vec= #v(0 0) (collide *poly-b* *poly-a*)))
  (setf (x *poly-a*) 17 (y *poly-a*) 20
        (x *poly-b*) 47 (y *poly-b*) 11)
  (assert-false (vec= #v(0 0) (collide *poly-b* *poly-a*)))
  (setf (x *poly-a*) 134 (y *poly-a*) 88
        (x *poly-b*) 118 (y *poly-b*) 85)
  (assert-vec-equal #v(3.5945957 -2.5675685) (collide *poly-a* *poly-b*)))

(def-test-method test-calc-motion ((test poly-test))
  (with-accessors ((a a) (v v) (s s)) *poly-a*
    (setf v #v(0 0) a #v(5 3))
    (calc-motion *poly-a* 1)
;    (assert-true (vec= #v(5 3) a (print v) s))
    (setf v #v(0 0) s #v(0 0))
    (calc-motion *poly-a* 2)
    ;; (assert-true (vec= v #v(10 6)) (dump-to-string v 'v))
    ;; (assert-true (vec= s #v(20 12)) (dump-to-string s 's))
    ))

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
                                     :polys `(,*poly-a* ,*poly-b* ,*poly-c*))))
      (click:add-root-listener m-control)
      (click:add-to-root a)
      (click:add-to-root b)
      (click:add-to-root c))))
