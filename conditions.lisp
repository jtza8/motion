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
