#|
This is the top-level file for the state logic, ie non-IO code.
|#

;;;Physics and astronomy code
;;All these two lines do is suppress meaningless compiler warnings.
(defgeneric objects (m))  ;Geodesic following code wants to be able to see a list of massive objects.
(defgeneric player (s)) (defgeneric v (p))  ;To account for moving reference frames in physics code.
(load "mathphysics.lisp")
(load "astrometry.lisp")

;;;Define some classes for maintaining the global program state.
;;The main objects of the application world, attributes tbd:
(defclass celestial-body (rigid-body)
  ())
(defclass sun (celestial-body)
  ())
(defclass planet (celestial-body)
  ())
(defclass moon (celestial-body)
  ())
(defclass laboratory (rigid-body)
  ())
(defclass vessel (rigid-body)
  ((v :accessor v :initarg :v :initform 0)))
(defclass player (vessel)
  ((phi :accessor phi :initarg :phi :initform 0)                  ;angle from roll to pitch axis
   (theta :accessor theta :initarg :theta :initform (/ pi 2))))   ;angle off yaw axis

;;Global state wrapper object. Obviously from all the hardcoding, I am still 
;;  missing a lot of astrometry data.
(defclass X_Orrery-state ()
  ((elapsed-time :reader elapsed-time :initarg :elapsed-time :initform 0)
   (last-internal-real-time :reader last-internal-real-time
			    :initarg :last-internal-real-time :initform (get-internal-real-time))
   (objects :reader objects :initarg :objects
	    :initform (list
		       ;;starfield (needs to be first...?)
		       (make-instance 'celestial-body
				      :name "Starfield"
				      :size 100000000)
		       ;;sun
		       (make-instance 'sun
				      :name "Sun" 
				      :mass sun-mass
				      :x (vector 0 0 (- earth-orbital-radius) 0)
				      :size sun-radius)
		       ;;mercury
		       (make-instance 'planet
				      :name "Mercury"
				      :mass mercury-mass
				      :x (vector 0 (- mercury-orbital-radius) (- earth-orbital-radius) 0)
				      :xdot (vector 0 0 mercury-orbital-speed 0)
				      :omega mercury-angular-velocity
				      :size mercury-radius)
		       ;;venus
		       (make-instance 'planet
				      :name "Venus"
				      :mass venus-mass
				      :x (vector 0 (* venus-orbital-radius 0.707)  (- (* venus-orbital-radius 0.707) earth-orbital-radius) 0)
				      :xdot (vector 0 (* venus-orbital-speed -0.717)  (* venus-orbital-speed 0.717) 0)
				      :omega venus-angular-velocity
				      :size venus-radius)
		       ;;earth
		       (make-instance 'planet
				      :name "Earth"
				      :mass earth-mass
				      :x (vector 0 1.3 0 0)
				      :xdot (vector 0 earth-orbit-speed 0 0)
				      :roll (vector 0 -1 0 0)
				      :pitch (vector 0 0 1 0)
				      :yaw (vector 0 0 0 -1)
				      :omega (- earth-angular-velocity)
				      :size earth-radius)
		       ;;mars
		       (make-instance 'planet
				      :name "Mars"
				      :mass mars-mass
				      :x (vector 0 0 (- mars-orbital-radius earth-orbital-radius) 0)
				      :xdot (vector 0 (- mars-orbital-speed) 0 0)
				      :omega mars-angular-velocity
				      :size mars-radius)
		       ;;jupiter
		       (make-instance 'planet
				      :name "Jupiter"
				      :mass jupiter-mass
				      :x (vector 0
						 (* -1 (sqrt 0.5) jupiter-orbital-radius)
						 (- (* (sqrt 0.5) jupiter-orbital-radius)  earth-orbital-radius)
						 0)
				      :xdot (vector 0
						    (* (sqrt 0.5) jupiter-orbital-speed)
						    (* -1 (sqrt 0.5) jupiter-orbital-speed)
						    0)
				      :omega jupiter-angular-velocity
				      :size jupiter-radius)
		       ;;saturn
		       (make-instance 'planet
				      :name "Saturn"
				      :mass saturn-mass
				      :x (vector 0
						 (* -1 (sqrt 0.5) saturn-orbital-radius)
						 (- (+ earth-orbital-radius (* (sqrt 0.5) saturn-orbital-radius)))
						 0)
				      :xdot (vector 0
						    (* -1 (sqrt 0.5) saturn-orbital-speed)
						    (* (sqrt 0.5) saturn-orbital-speed)
						    0)
				      :roll (vector 0 0 (sqrt 0.5) (sqrt 0.5))
				      :pitch (vector 0 1 0 0)
				      :yaw (vector 0 0 (sqrt 0.5) (- (sqrt 0.5)))
				      :omega saturn-angular-velocity
				      :size saturn-radius)
		       ;;uranus
		       (make-instance 'planet
				      :name "Uranus"
				      :mass uranus-mass
				      :x (vector 0
						 (* (sqrt 0.5) uranus-orbital-radius)
						 (- (* (sqrt 0.5) uranus-orbital-radius) earth-orbital-radius)
						 0)
				      :xdot (vector 0
						    (* -1 (sqrt 0.5) uranus-orbital-speed)
						    (* (sqrt 0.5) uranus-orbital-speed)
						    0)
				      :omega uranus-angular-velocity
				      :size uranus-radius)
		       ;;neptune
		       (make-instance 'planet
				      :name "Neptune"
				      :mass neptune-mass
				      :x (vector 0
						 (* (sqrt 0.5) neptune-orbital-radius)
						 (- (- (sqrt 0.5) neptune-orbital-radius) earth-orbital-radius)
						 0)
				      :xdot (vector 0
						    (* (sqrt 0.5) neptune-orbital-speed)
						    (* -1 (sqrt 0.5) neptune-orbital-speed)
						    0)
				      :omega neptune-angular-velocity
				      :size neptune-radius)
		       ;;moon
		       (make-instance 'moon 
				      :name "Moon"
				      :mass moon-mass
				      :x (vector 0   (- 1.3 moon-orbital-radius) 0.007 0)
				      :xdot (vector 0  earth-orbit-speed (- moon-orbit-speed) 0 0)
				      :omega earth-angular-velocity
				      :size moon-radius
				      )
		       ;;international space station
		       (make-instance 'laboratory
				      :name "Low Earth Orbiter"
				      :mass 0 
				      :x (vector 0 1.3 (- low-earth-orbital-radius) 0)
				      :xdot (vector 0 earth-orbit-speed 0 low-earth-orbit-speed)
				      :omega earth-angular-velocity
				      :size (* 1000000 meter)  ;not sure why this has to be so big?
				      )
		       
		       )
	    )
   (player :accessor player :initarg :player
	   :initform (make-instance 'player :mass 0 :name "Player"))))



;;;This updates global state given new controller state and the old state.
;;It's a pure function for thread safety and overall robustitude.
(defun get-new-state (old-state controller time)
  ;;(debugvar "called new state with mouse-keys" (mouse-keys controller))
  (make-instance 'X_Orrery-state
		 :elapsed-time (+ (elapsed-time old-state) (- time (last-internal-real-time old-state)))
		 :last-internal-real-time time
		 :objects (mapcar (compose (lambda (p) (follow-geodesic
						   p
						   (/ (- time (last-internal-real-time old-state)) 1000.0)
						   old-state))
					   (lambda (p) (rotate
						   p
						   #(0 1 0 0)  ;always the player's roll axis
						   (roll-rate controller)))
					   (lambda (p) (rotate
						   p
						   #(0 0 1 0)  ;always the player's pitch axis
						   (* (pitch-rate controller)
						      (max (expt 2 (- (v (player old-state)))) 0.001))))
					   (lambda (p) (rotate
						   p
						   #(0 0 0 1)
						   (* (yaw-rate controller)
						      (max (expt 2 (- (v (player old-state)))) 0.001)))))
				  (objects old-state))
		 :player (copy-instance (player old-state)
					;;update phi and theta, shifting them back to their usual domains if needed
					:phi (let* ((old-phi (phi (player old-state)))
						    (dphi 0.007)  ;mouse sensitivity
						    (delta-phi (* -1 (mouse-deltax controller) dphi)))
					       (cond ((> (+ old-phi delta-phi) (* 2 pi) )
						      (- (+ old-phi delta-phi) (* 2 pi)))
						     ((< (+ old-phi delta-phi) 0)
						      (+ old-phi delta-phi (* 2 pi)))
						     (t
						      (+ old-phi delta-phi))))
					:theta (let* ((old-theta (theta (player old-state)))
						      (dtheta 0.007)  ;mouse sensitivity
						      (delta-theta (* (mouse-deltay controller) dtheta)))
						 (cond ((> (+ old-theta delta-theta) pi)
							pi)
						       ((< (+ old-theta delta-theta) 0)
							0)
						       (t
							(+ old-theta delta-theta))))
					:v (cond ((assoc #\b (key-down-time-table controller))
						  250)
						 ((member 'control-wheel-up (mouse-events controller))
						  (+ (v (player old-state))
						     (* 0.00001 (- c (v (player old-state))))))
						 ((member 'wheel-up (mouse-events controller))
						  (+ (v (player old-state))
						     (* 0.001 (- c (v (player old-state))))))
						 ((member 'shift-wheel-up (mouse-events controller))
						  (+ (v (player old-state))
						     (* 0.05 (- c (v (player old-state))))))
						 ((member 'control-wheel-down (mouse-events controller))
						  (* (v (player old-state)) 0.9))
						 ((member 'wheel-down (mouse-events controller))
						  (* (v (player old-state)) 0.6))
						 ((member 'shift-wheel-down (mouse-events controller))
						  0)
						 (t (v (player old-state)))))))

