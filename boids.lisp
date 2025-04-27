;;;; boids.lisp

(in-package #:flock-simulator)

(defclass boid ()
  ((location :initarg :location :accessor location)
   (velocity :initarg :velocity :initform (3dv:vec2) :accessor velocity)))

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

(defgeneric update-location (object boids))
(defgeneric update-velocity (object boids))
(defgeneric rule (object boids number))

(defmethod update-location ((boid boid) boids)
  (with-accessors ((velocity velocity) (location location)) boid
    (setf location (3dv:v+ location velocity))))

(defmethod update-velocity ((boid boid) boids)
  (let ((max-velocity 10.0))
    (with-accessors ((velocity velocity) (location location)) boid
      (setf velocity (3dv:vlimit (3dv:v+ velocity
                                         (rule boid boids 1)
                                         (rule boid boids 2)
                                         (rule boid boids 3))
                                 max-velocity)))))

(defmethod rule ((boid boid) boids (number (eql 1)))
  (declare (ignore number))
  (loop with v-sum = (3dv:vec2)
        for boid1 in boids
        for offset = (3dv:v- (location boid) (location boid1))
        for distance = (3dv:vlength offset)
        if (and (not (eq boid boid1)) (< distance 10))
          do (3dv:nv+ v-sum offset)
        finally (return (if (zerop (3dv:vlength v-sum))
                            v-sum
                            (3dv:nvscale v-sum (/ 1.0))))))

(defmethod rule ((boid boid) boids (number (eql 2)))
  (declare (ignore number))
  (loop with center = (3dv:vec2)
        for boid1 in boids
        if (not (eq boid boid1))
          do (3dv:nv+ center (location boid1))
        finally (3dv:nvscale center (/ (1- (length boids))))
                (3dv:nv- center (location boid))
                (3dv:nvscale center (/ 5.0))
                (return center)))

(defmethod rule ((boid boid) boids (number (eql 3)))
  (declare (ignore number))
  (loop with result = (3dv:vec2)
        for boid1 in boids
        if (not (eq boid boid1))
          do (3dv:nv+ result (velocity boid1))
        finally (let ((rl (3dv:vlength result)))
                  (unless (zerop rl)
                    (3dv:nvscale result (/ (1- (length boids))))
                    (3dv:nv- result (velocity boid))
                    (3dv:nvscale result (/ 8.0)))
                  (return result))))

(defmethod rule ((boid boid) boids (number (eql 3)))
  (declare (ignore number))
  (3dv:vec2))
