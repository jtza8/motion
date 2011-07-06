; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass motion-control (listener)
  ((polys :initform '()
             :initarg :polys)
   (cell-width :initform 128)
   (cell-height :initform 128)
   (rows :initform 20)
   (columns :initform 100)
   (dirty-cells :initform '())
   cell
   cells))

(defmethod initialize-instance :after ((control motion-control) &key)
  (with-slots (cell polys) control
    (setf cell (make-instance 'collision-cell :polys polys))
    (desire-events control :loop-iteration #'loop-iteration-handler)))

(defmethod loop-iteration-handler ((control motion-control) event)
  (with-slots (polys cell) control
   (events:with-event-keys (time) event
      (dolist (poly polys)
        (calc-motion poly time))
      (detect-collisions cell)
      (dolist (poly polys)
        (displace poly))
      ))
  )

;; (defmethod initialize-instance :after ((control motion-control) &key)
;;   (with-slots (cells dirty-cells rows columns) control
;;     (setf cells (make-array (list rows columns) :element-type 'poly))
;;     (dotimes (i columns)
;;       (dotimes (j columns)
;;         (push (setf (aref cells i j)
;;                     (make-instance 'collision-cell))
;;               dirty-cells)))))

;; (defmethod calc-collisions ((control motion-control))
;;   (with-slots (dirty-cells) control
;;     (dolist (cell dirty-cells)
;;       (detect-collisions cell))))

;; (defmethod cell-at ((control motion-control) x y)
;;   (with-slots (cells cell-width cell-height rows columns) control
;;     (let ((column (truncate (/ x cell-width)))
;;           (row (truncate (/ y cell-height))))
;;           (when (and (< -1 row rows) (< -1 column columns))
;;             (aref cells column row)))))

;; (defmethod poly-motion-handler ((control motion-control) event)
;;   (with-slots (cells cell-width cell-height rows columns) control
;;     (with-event-keys (poly dx dy) event
;;       (let ((old-cell (cell-at control (x poly) (y poly)))
;;             (new-cell (cell-at control (+ (x poly) dx) (+ (y poly) dy))))
;;         (unless (null old-cell) (remove-poly old-cell poly))
;;         (unless (null new-cell) (add-poly new-cell poly))))))


;; (defmethod calc-motion ((control motion-control) time)
;;   (with-slots (cell polys) control
;;     (dolist (poly polys)
;;       (calc-motion poly time))
;;     (detect-collisions cell)
;;     (dolist (poly polys)
;;       (displace poly))))