; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:motion
  (:use #:cl #:meta-package #:events))

(in-package :motion)
(defconstant +plus-infinity+
  #+sbcl (sb-kernel:make-single-float #x7F800000)
  #+ccl 1D++0)
(defconstant +minus-infinity+ (- +plus-infinity+))