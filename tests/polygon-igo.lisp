; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass polygon-igo (click:igo)
  ((polygon :initarg :polygon
            :initform (error "Must specify polygon."))
   (dragable :initarg :dragable
             :initform t
             :accessor dragable)
   (drag-state :initform nil)
   (collision-state :initform nil)
   drag-offset normal-sprite collision-sprite))

(click:define-instance-maker polygon-igo)

(defmethod initialize-instance :after ((igo polygon-igo) &key)
  (with-slots (polygon width height) igo
    (setf (click:x igo) (x polygon)
          (click:y igo) (y polygon))
    (update-sprites igo)
    (desire-events igo :mouse-motion #'handle-mouse-events
                   :mouse-button-up #'handle-mouse-events
                   :mouse-button-down #'handle-mouse-events
                   :collision #'handle-collision-events
                   :after-frame #'handle-after-frame-events)
    (subscribe polygon igo)))

(defmethod handle-mouse-events ((igo polygon-igo) event)
  (with-slots (dragable polygon drag-state drag-offset) igo
    (unless dragable
      (return-from handle-mouse-events))
    (with-event-keys (x y) event
      (case (event-type event)
        (:mouse-button-down
         (when (click:within igo x y)
           (setf drag-offset (vec (- x (click:x igo))
                                  (- y (click:y igo)))
                 drag-state t
                 (fixed polygon) t)))
        (:mouse-button-up
         (when (click:within igo x y)
           (setf drag-state nil
                 (fixed polygon) nil)))
        (:mouse-motion
         (when drag-state
           (setf (click:x igo) (- x (x drag-offset))
                 (click:y igo) (- y (y drag-offset))
                 (x polygon) (- x (x drag-offset))
                 (y polygon) (- y (y drag-offset)))))))))

(defmethod handle-collision-events ((igo polygon-igo) event)
  (declare (ignore event))
  (with-slots (collision-state) igo
    (setf collision-state t)))

(defmethod handle-after-frame-events ((igo polygon-igo) event)
  (declare (ignore event))
  (with-slots (polygon click:x click:y) igo
;    (calc-motion polygon (click:lap click:*iter-watch* :sec))
    (setf click:x (truncate (x polygon))
          click:y (truncate (y polygon)))))

(defmethod update-sprites ((igo polygon-igo))
  (with-slots (normal-sprite collision-sprite polygon) igo
    (setf normal-sprite (click:make-polygon-sprite
                          :points (map 'vector #'vec-vector (points polygon))
                          :width (click:width igo)
                          :height (click:height igo)
                          :line-colour '(0 0 0 1)
                          :line-width 2
                          :fill-colour '(0.0 0.8 0.0 1))
          collision-sprite (click:diverge normal-sprite 
                                          :fill-colour '(0.8 0.0 0.0 1)))))

(defmethod click:draw ((igo polygon-igo))
  (with-slots (collision-state normal-sprite collision-sprite) igo
    (click:draw-sprite (if collision-state
                           collision-sprite
                           normal-sprite))
    (setf collision-state nil)))