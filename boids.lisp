;;;; boids.lisp

(in-package #:flock-simulator)

(defclass boid ()
  ((location :initarg :location :accessor location)
   (velocity :initarg :velocity :initform (3dv:vec2 0.0 0.0) :accessor velocity)))

(defgeneric draw (object &optional pane &rest drawing-options))

(defmethod draw ((boid boid) &optional (pane *standard-output*) &rest drawing-options)
  (let ((boid-width 20.0)
        (boid-length 40.0))
    (with-accessors ((velocity velocity) (location location)) boid
      (let* ((direction (if (zerop (3dv:vlength velocity))
                            (3dv:vec2 0.0 -1.0)
                            (3dv:v/ velocity (3dv:v2norm velocity))))
             (p0 (3dv:v+ location
                         (3dv:vscale direction (/ boid-length 2.0))))
             (p1 (3dv:v+ location
                         (3dv:vscale direction (- (/ boid-length 2.0)))
                         (3dv:vscale (3dv:vrot2 direction (/ pi 2)) (/ boid-width 2.0))))
             (p2 (3dv:v+ location
                         (3dv:vscale direction (- (/ boid-length 2.0)))
                         (3dv:vscale (3dv:vrot2 direction (/ pi -2)) (/ boid-width 2.0)))))
        (apply #'draw-polygon* pane (list (3dv:vx p0) (3dv:vy p0)
                                          (3dv:vx p1) (3dv:vy p1)
                                          (3dv:vx p2) (3dv:vy p2))
               drawing-options)))))


(defgeneric update (object))

(defmethod update ((boid boid))
  (declare (ignore boid))
  t)
