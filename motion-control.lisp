; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :motion)

(defclass motion-control (listener)
  ((objects :initform '()
            :initarg :objects)
   (cell-width :initform 128)
   (cell-height :initform 128)
   (rows :initform 20)
   (columns :initform 100)
   (dirty-cells :initform '())
   cell
   cells))

(defmethod initialize-instance :after ((control motion-control) &key)
  (with-slots (objects cell) control
    (setf cell (make-instance 'collision-cell :matters objects))
    (desire-events control :loop-iteration #'loop-iteration-handler)))

(defmethod loop-iteration-handler ((control motion-control) event)
  (with-slots (cell) control
    (events:with-event-keys (time) event
      (apply-matter-physics cell time))))

;; (defmethod initialize-instance :after ((control motion-control) &key)
;;   (with-slots (cells dirty-cells rows columns) control
;;     (setf cells (make-array (list rows columns) :element-type 'object))
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

;; (defmethod object-motion-handler ((control motion-control) event)
;;   (with-slots (cells cell-width cell-height rows columns) control
;;     (with-event-keys (object dx dy) event
;;       (let ((old-cell (cell-at control (x object) (y object)))
;;             (new-cell (cell-at control (+ (x object) dx) (+ (y object) dy))))
;;         (unless (null old-cell) (remove-object old-cell object))
;;         (unless (null new-cell) (add-object new-cell object))))))


;; (defmethod calc-motion ((control motion-control) time)
;;   (with-slots (cell objects) control
;;     (dolist (object objects)
;;       (calc-motion object time))
;;     (detect-collisions cell)
;;     (dolist (object objects)
;;       (displace object))))