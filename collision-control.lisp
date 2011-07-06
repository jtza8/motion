; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

; Collision Control, just like Air Traffic Control, or Mission Control only not.

(in-package :motion)

(defclass collision-control (listener)
  ((cells :initform '()
          :initarg :cells
          :accessor cells)))

(defmethod initialize-instance :after ((control collision-control) &key)
  (desire-events control :before-frame (no-event-arg #'detect-collisions)))

(defmethod detect-collisions ((control collision-control))
  ())