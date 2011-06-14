; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(internal reason-reporter)
(defmacro reason-reporter (condition stream reason-slot (&rest slots)
                           &body cases)
  `(with-slots (,reason-slot ,@slots) ,condition
     (format ,stream
             (ecase ,reason-slot
               ,@(loop for case in cases
                       if (> (length case) 2)
                         collect `(,(car case)
                                    (format nil ,@(subseq case 1)))
                       else
                         collect case)))))

(define-condition vec-error (error)
  ((reason :initarg :reason
           :initform nil
           :reader reason)
   (vec-one :initarg :vec
            :reader vec-one)
   (vec-two :initarg :vec-two
            :reader vec-two))
  (:report (lambda (condition stream)
             (reason-reporter condition stream reason (vec-one vec-two)
               (:dimensionality
                "Vecs ~s and ~s are of different dimensions." vec-one vec-two)
               (:2d-only "3D vec ~s not alowed." vec-one)))))

(defun check-vec-dimensions (a &rest vecs)
  (loop with check = (if (null (c a)) #'null (complement #'null))
        with dimensions = (if (null (c a)) 2 3)
        for b in vecs
        unless (funcall check (c b))
          do (error 'vec-error :reason :dimensionality :vec a :vec b)
        finally (return dimensions)))

(defun assert-vecs-2d (&rest vecs)
  (loop for vec in vecs
        unless (null (c vec))
          do (error 'vec-error :reason :2d-only :vec vec)))
