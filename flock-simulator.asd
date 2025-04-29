;;;; flock-simulator.asd
;;;; Copyright 2025 José M. Á. Ronquillo Rivera

(asdf:defsystem #:flock-simulator
  :description "Describe flock-simulator here"
  :author "José M. Á. Ronquillo Rivera <jose@rufina.link>"
  :license  "GPLv3"
  :version "0.1"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:bordeaux-threads
               #:local-time
               #:3d-matrices
               #:3d-vectors
               #:mcclim)
  :components ((:file "package")
               (:file "boids")
               (:file "flock-simulator")))
