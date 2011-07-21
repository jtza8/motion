; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass poly-igo (click:igo)
  ((poly :initarg :poly
            :initform (error "Must specify poly."))
   (dragable :initarg :dragable
             :initform t
             :accessor dragable)
   (drag-state :initform nil)
   (collision-state :initform nil)
   drag-offset normal-sprite collision-sprite))

(click:define-instance-maker poly-igo)

(defmethod initialize-instance :after ((igo poly-igo) &key)
  (with-slots (poly width height) igo
    (setf (click:x igo) (x poly)
          (click:y igo) (y poly))
    (update-sprites igo)
    (desire-events igo :mouse-motion #'handle-mouse-events
                   :mouse-button-up #'handle-mouse-events
                   :mouse-button-down #'handle-mouse-events
                   :collision #'handle-collision-events
                   :after-frame #'handle-after-frame-events)
    (subscribe poly igo)))

(defmethod handle-mouse-events ((igo poly-igo) event)
  (with-slots (dragable poly drag-state drag-offset) igo
    (unless dragable
      (return-from handle-mouse-events))
    (with-event-keys (x y) event
      (case (event-type event)
        (:mouse-button-down
         (when (click:within igo x y)
           (setf drag-offset (vector (- x (click:x igo))
                                  (- y (click:y igo)))
                 drag-state t)))
        (:mouse-button-up
         (when (click:within igo x y)
           (setf drag-state nil)))
        (:mouse-motion
         (when drag-state
           (setf (click:x igo) (- x (x drag-offset))
                 (click:y igo) (- y (y drag-offset))
                 (x poly) (- x (x drag-offset))
                 (y poly) (- y (y drag-offset)))))))))

(defmethod handle-collision-events ((igo poly-igo) event)
  (declare (ignore event))
  (with-slots (collision-state) igo
    (setf collision-state t)))

(defmethod handle-after-frame-events ((igo poly-igo) event)
  (declare (ignore event))
  (with-slots (poly click:x click:y) igo
;    (calc-motion poly (click:lap click:*iter-watch* :sec))
    (setf click:x (truncate (x poly))
          click:y (truncate (y poly)))))

(defmethod update-sprites ((igo poly-igo))
  (with-slots (normal-sprite collision-sprite poly) igo
    (setf normal-sprite (click:make-polygon-sprite
                          :points (map 'vector #'vec-vector (points poly))
                          :width (click:width igo)
                          :height (click:height igo)
                          :line-colour '(0 0 0 1)
                          :line-width 2
                          :fill-colour '(0.0 0.8 0.0 1))
          collision-sprite (click:diverge normal-sprite 
                                          :fill-colour '(0.8 0.0 0.0 1)))))

(defmethod click:draw ((igo poly-igo))
  (with-slots (collision-state normal-sprite collision-sprite) igo
    (click:draw-sprite (if collision-state
                           collision-sprite
                           normal-sprite))
    (setf collision-state nil)))