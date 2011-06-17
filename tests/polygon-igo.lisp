; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defparameter *polygons* '())

(defclass polygon-igo (click:igo)
  ((polygon :initarg :polygon
            :initform (error "Must specify polygon."))
   (drag-state :initform nil)
   drag-offset normal-sprite collision-sprite))

(click:define-instance-maker polygon-igo)

(defmethod initialize-instance :after ((igo polygon-igo) &key)
  (with-slots (polygon x y width height) igo
    (setf (click:x igo) (x polygon)
          (click:y igo) (y polygon)))
  (update-sprites igo)
  (click:desire-events igo :mouse-motion #'handle-mouse-events
                       :mouse-button-up #'handle-mouse-events
                       :mouse-button-down #'handle-mouse-events))

(defmethod handle-mouse-events ((igo polygon-igo) event)
  (with-slots (polygon drag-state drag-offset) igo
    (click:with-event-keys (x y) event
      (case (click:event-type event)
        (:mouse-button-down
         (when (click:within igo x y)
           (setf drag-offset (vec (- x (click:x igo))
                                  (- y (click:y igo)))
                 drag-state t)))
        (:mouse-button-up
         (setf drag-state nil))
        (:mouse-motion
         (when drag-state
           (setf (click:x igo) (- x (x drag-offset))
                 (click:y igo) (- y (y drag-offset))
                 (x polygon) x
                 (y polygon) y)))))))

(defmethod update-sprites ((igo polygon-igo))
  (with-slots (normal-sprite collision-sprite polygon) igo
    (setf normal-sprite (click:make-polygon-sprite
                          :points (map 'vector #'vec-vector (points polygon))
                          :width 64
                          :height 64
                          :line-colour '(0 0 0 1)
                          :line-width 2
                          :fill-colour '(0.0 0.8 0.0 1))
          collision-sprite (click:diverge normal-sprite 
                                          :fill-colour '(0.8 0.0 0.0 1)))))

(defmethod click:draw ((igo polygon-igo))
  (with-slots (polygon normal-sprite collision-sprite) igo
    (loop for other-polygon in *polygons*
          when (and (not (eq polygon other-polygon))
                    (collides-p polygon other-polygon))
            return (click:draw-sprite collision-sprite)
          finally (click:draw-sprite normal-sprite))))