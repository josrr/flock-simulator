;;;; flock-simulator.lisp

(in-package #:flock-simulator)

(defun display-canvas (frame pane)
  (with-bounding-rectangle* (x0 y0 x1 y1) pane
    ;;(draw-rectangle* pane x0 y0 x1 y1 :ink +white+)
    (with-drawing-options (pane :transformation (make-translation-transformation (/ (+ x0 x1) 2.0)
                                                                                 (/ (+ y0 y1) 2.0)))
      (draw-point* pane 0.0 0.0 :ink +red+ :line-thickness 25)
      (dolist (boid (boids frame))
        (draw boid pane :ink +blue+))
      (dolist (boid (boids frame))
        (update-location boid (boids frame)))
      (dolist (boid (boids frame))
        (update-velocity boid (boids frame))))))


;;; Sheets and events.
(defclass canvas-pane (clim-stream-pane)
  ((pulse-running-p :initform nil :accessor pulse-running-p))
  (:default-initargs :display-function 'display-canvas
                     :region (make-rectangle* 0 0 200 200)))

(defun dispatch-redisplay (pane)
  (with-output-recording-options (pane :draw nil :record t)
    (clear-output-record (stream-output-history pane))
    (funcall (climi::pane-display-function pane)
             (pane-frame pane) pane))
  (dispatch-repaint pane +everywhere+))

(defun toggle-pulse (pane)
  (setf (pulse-running-p pane)
        (not (pulse-running-p pane)))
  (when (pulse-running-p pane)
    (clime:schedule-pulse-event pane :boom 0.001))
  (format *debug-io* "pulse-running-p: ~a~%" (pulse-running-p pane)))

(defun handle-pulse (pane)
  (dispatch-redisplay pane))

(defmethod handle-event ((self canvas-pane) (event climi::pulse-event))
  (if (and (pulse-running-p self)
           (sheet-grafted-p self))
      (handle-pulse self)
      (clime:delete-pulse-event event)))

(defmethod handle-event ((self canvas-pane) (event pointer-button-press-event))
  (toggle-pulse self))

(defmethod compose-space ((self canvas-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 200 :height 200))


;;; Application frame
(define-application-frame flock-simulator ()
  ((boids :initarg :boids :initform nil :accessor boids))
  (:panes (canvas (make-pane 'canvas-pane
                             :name 'canvas
                             :background +white+))
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height 1280 :max-height 1280 :height 1280
                          :min-width 1024 :max-width 1024 :width 1024)
               (4/5 canvas)
               (1/5 interactor))))
  (:menu-bar t)
  (:reinitialize-frames t))

(defmethod run-frame-top-level :before ((frame flock-simulator) &key &allow-other-keys)
  (let ((canvas (find-pane-named frame 'canvas)))
    (with-bounding-rectangle* (x0 y0 x1 y1) canvas
      (let ((width (- x1 x0))
            (height (- y1 y0)))
       (setf (boids frame) (loop repeat 40
                                 collect (make-instance 'boid
                                                        :location (3dv:vec2 (- (random width)
                                                                               (/ width 2))
                                                                            (- (random height)
                                                                               (/ height 2))))))))))

(defun start ()
  (find-application-frame 'flock-simulator))
