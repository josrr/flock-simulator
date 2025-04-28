;;;; flock-simulator.lisp

(in-package #:flock-simulator)

(defun display-canvas (frame pane)
  (with-bounding-rectangle* (x0 y0 x1 y1) pane
    (let ((dx (/ (+ x0 x1) 2.0))
          (dy (/ (+ y0 y1) 2.0)))
      (with-drawing-options (pane :transformation (make-translation-transformation dx dy))
        (draw-point* pane 0.0 0.0 :ink +red+ :line-thickness 25)
        (dolist (boid (boids frame))
          (draw boid pane :ink +blue+))
        (dolist (boid (boids frame))
          (update-location boid (boids frame)))
        (dolist (boid (boids frame))
          (update-velocity boid (boids frame) (3dv:v- (destination frame) (3dv:vec2 dx dy))))))))


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
    (clime:schedule-pulse-event pane :boom (/ 60.0)))
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

(defmethod handle-event ((self canvas-pane) (event pointer-motion-event))
  (with-application-frame (frame)
    (setf (destination frame) (3dv:vec2 (pointer-event-x event) (pointer-event-y event)))))

(defmethod compose-space ((self canvas-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 200 :height 200))


;;; Application frame
(define-application-frame flock-simulator ()
  ((boids :initarg :boids :initform nil :accessor boids)
   (destination :initform (3dv:vec2 0.0 0.0) :accessor destination))
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

(defun create-boids (frame &optional redisplayp)
  (let ((canvas (find-pane-named frame 'canvas)))
    (with-bounding-rectangle* (x0 y0 x1 y1) canvas
      (let ((width (- x1 x0))
            (height (- y1 y0)))
        (setf (boids frame) (loop repeat 50
                                  collect (make-instance 'boid
                                                         :location (3dv:vec2 (- (random width)
                                                                                (/ width 2))
                                                                             (- (random height)
                                                                                (/ height 2))))))))
    (when redisplayp
      (setf (pane-needs-redisplay (find-pane-named frame 'canvas)) t))))

(defmethod run-frame-top-level :before ((frame flock-simulator) &key &allow-other-keys)
  (create-boids frame))

(define-flock-simulator-command (com-reset :name "Reset" :menu t) ()
  (with-application-frame (frame)
    (create-boids frame t)))

(defun start ()
  (find-application-frame 'flock-simulator))
