;;;; boids.lisp
;;;; Copyright 2025 José M. Á. Ronquillo Rivera

(in-package #:flock-simulator)

(defclass boid ()
  ((location :initarg :location :accessor location)
   (velocity :initarg :velocity :initform (3dv:vec2 (random 10.0) (random 10.0)) :accessor velocity)))

(defgeneric draw (object &optional pane &rest drawing-options))

(defmethod draw ((boid boid) &optional (pane *standard-output*) &rest drawing-options)
  (let ((boid-width 20.0)
        (boid-length 40.0))
    (with-accessors ((velocity velocity) (location location)) boid
      (let* ((direction (if (zerop (3dv:vlength velocity))
                            (3dv:vec2 0.0 -1.0)
                            (3dv:v/ velocity (3dv:v2norm velocity))))
             (p0 (3dv:v+ location
                         (3dv:v* direction boid-length 0.5)))
             (p1 (3dv:v+ location
                         (3dv:v* direction boid-length -0.5)
                         (3dv:v* (3dv:vrot2 direction (/ pi 2)) boid-width 0.5)))
             (p2 (3dv:v+ location
                         (3dv:v* direction boid-length -0.5)
                         (3dv:v* (3dv:vrot2 direction (/ pi -2)) boid-width 0.5))))
        (apply #'draw-polygon* pane (list (3dv:vx p0) (3dv:vy p0)
                                          (3dv:vx p1) (3dv:vy p1)
                                          (3dv:vx p2) (3dv:vy p2))
               drawing-options)))))

(defgeneric update-location (object))
(defgeneric update-velocity (object boids destination))
(defgeneric rule (object boids number))

(defmethod update-location ((boid boid))
  (setf (location boid) (3dv:v+ (location boid) (velocity boid))))

(defparameter *max-velocity* 10.0)

(defmethod update-velocity ((boid boid) boids destination)
  (with-accessors ((velocity velocity)) boid
    (setf velocity (3dv:vlimit (3dv:v+ velocity
                                       (rule boid boids 1)
                                       (rule boid boids 2)
                                       (rule boid boids 3)
                                       (3dv:vscale (3dv:v- destination (location boid)) 0.8))
                               *max-velocity*))))

(defmethod rule ((boid boid) boids (number (eql 1)))
  (declare (ignore number))
  (loop with v-sum = (3dv:vec2)
        for boid1 in boids
        for offset = (3dv:v- (location boid1) (location boid))
        for distance = (3dv:vlength offset)
        if (and (not (eq boid boid1)) (< distance 20.0))
          do (3dv:nv- v-sum offset)
        finally (return v-sum)))

(defmethod rule ((boid boid) boids (number (eql 2)))
  (declare (ignore number))
  (loop with center = (3dv:vec2)
        for boid1 in boids
        if (not (eq boid boid1))
          do (3dv:nv+ center (location boid1))
        finally (return (3dv:nv/ (3dv:nv- (3dv:nv/ center (1- (length boids)))
                                          (location boid))
                                 200.0))))

(defmethod rule ((boid boid) boids (number (eql 3)))
  (declare (ignore number))
  (loop with result = (3dv:vec2)
        for boid1 in boids
        if (not (eq boid boid1))
          do (3dv:nv+ result (velocity boid1))
        finally (return (3dv:nv/ (3dv:nv- (3dv:nv/ result (1- (length boids)))
                                          (velocity boid))
                                 10.0))))
