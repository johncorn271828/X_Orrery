#|
This has some useful math and physics code I wrote for simulating the solar system and changing
reference frames.
|#

;;;Vector math
;;Rotate the space components of the 4-vector 4vect around the axis specified by the 4-vector n by angle theta.
;;See http://inside.mines.edu/fs_home/gmurray/ArbitraryAxisRotation/
(defun rotate-4-vector (4vect n theta)
  (if (= 0 theta)
      4vect
      (let* ((length-n (sqrt
			(+ (expt (aref n 1) 2) (expt (aref n 2) 2) (expt (aref n 3) 2))))
	     (u (/ (aref n 1) length-n))
	     (v (/ (aref n 2) length-n))
	     (w (/ (aref n 3) length-n))
	     (x (aref 4vect 1))
	     (y (aref 4vect 2))
	     (z (aref 4vect 3))
	     (result))
	;;(princln "doing some vector rotating")
	(setf result (vector (aref 4vect 0)
			     (+ (* u (+ (* u x) (* v y) (* w z) ) (- 1 (cos theta)))
				(* x (cos theta))
				(* (- (* v z) (* w y)) (sin theta)))
			     (+ (* v (+ (* u x) (* v y) (* w z) ) (- 1 (cos theta)))
				(* y (cos theta))
				(* (- (* w x) (* u z)) (sin theta)))
			     (+ (* w (+ (* u x) (* v y) (* w z)) (- 1 (cos theta)))
				(* z (cos theta))
				(* (- (* u y) (* v x)) (sin theta)))))
	;;(debugvar "rotated vector" result)
	result)))

;;;Physics stuff.
;;Physical constants in kilogram light-second second units.
(defconstant meter 3.33564e-9)
(defconstant G 2.477e-36)
(defconstant c 1)

;;This class handles degrees of freedom of a rigid body, with a generic "size" variable and some
;; other redundancies for coding convenience.
(defclass rigid-body ()
  ((name :accessor name :initarg :name :initform "")
   (mass :accessor mass :initarg :mass :initform 0)
   (x :accessor x :initarg :x :initform (vector 0d0 0d0 0d0 0d0))           ;position 4-vector
   (xdot :accessor xdot :initarg :xdot :initform (vector 0d0 0d0 0d0 0d0))  ;and its derivatives wrt player time
   (xdotdot :accessor xdotdot :initarg :xdotdot :initform (vector 0 0 0 0))
   (roll :reader roll :initarg :roll :initform (vector 0 1 0 0))
   (pitch :reader pitch :initarg :pitch :initform (vector 0 0 1 0))
   (yaw :reader yaw :initarg :yaw :initform (vector 0 0 0 1)) ;Don't technically need this.
   (omega :reader omega :initarg :omega :initform 0)
   (size :reader size :initarg :size :initform 0)))
;;This method rotates a rigid body object's vectors around a vector n by an angle theta.
(defgeneric rotate (x n theta))
(defmethod rotate ((rb rigid-body) n theta)
  (copy-instance rb
		 :x (rotate-4-vector (x rb) n theta)
		 :xdot (rotate-4-vector (xdot rb) n theta)
		 :xdotdot (rotate-4-vector (xdotdot rb) n theta)
		 :roll (rotate-4-vector (roll rb) n theta)      ;rotation axis
		 :pitch (rotate-4-vector (pitch rb) n theta)
		 :yaw (rotate-4-vector (yaw rb) n theta)))
;;Use Newtonian gravity (for now) to simulate planetary motion.
(defgeneric follow-geodesic (p deltat old-state)) ;deltat is small change in player's rf time
(defmethod follow-geodesic ((p rigid-body) deltat old-state)
  (copy-instance p
		 :x (vector (+ (aref (x p) 0) (* deltat (aref (xdot p) 0)))
			    (+ (aref (x p) 1) (* deltat (aref (xdot p) 1))
			       (* -1 deltat (v (player old-state)))   ) 
			    (+ (aref (x p) 2) (* deltat (aref (xdot p) 2)))
			    (+ (aref (x p) 3) (* deltat (aref (xdot p) 3))))
		 :xdot (vector (expt (- 1 ( / (+ (expt (aref (xdot p) 1) 2)
						 (expt (aref (xdot p) 2) 2)
						 (expt (aref (xdot p) 3) 2))
					      (expt c 2)))
				     -0.5) ;aka gamma
			       (+ (aref (xdot p) 1) (* deltat (aref (xdotdot p) 1)))
			       (+ (aref (xdot p) 2) (* deltat (aref (xdotdot p) 2)))
			       (+ (aref (xdot p) 3) (* deltat (aref (xdotdot p) 3))))
		 ;;for now, Newtonian gravity
		 :xdotdot (vector 0
				  (loop for object in (objects old-state)
				     sum (if (not (equal (name object) (name p)))
					     (* G
						(mass object)
						(- (aref (x object) 1) (aref (x p) 1))
						(expt (+
						       (expt  (- (aref (x object) 1) (aref (x p) 1))  2)
						       (expt  (- (aref (x object) 2) (aref (x p) 2))  2)
						       (expt  (- (aref (x object) 3) (aref (x p) 3))  2)
						       )  -1.5))
					     0.0))
				  (loop for object in (objects old-state)
				     sum (if (not (equal (name object) (name p)))
					     (* G
						(mass object)
						(- (aref (x object) 2) (aref (x p) 2))
						(expt (+
						       (expt  (- (aref (x object) 1) (aref (x p) 1))  2)
						       (expt  (- (aref (x object) 2) (aref (x p) 2))  2)
						       (expt  (- (aref (x object) 3) (aref (x p) 3))  2)
						       )  -1.5))
					     0.0))
				  (loop for object in (objects old-state)
				     sum (if (not (equal (name object) (name p)))
					     (* G
						(mass object)
						(- (aref (x object) 3) (aref (x p) 3))
						(expt (+
						       (expt  (- (aref (x object) 1) (aref (x p) 1))  2)
						       (expt  (- (aref (x object) 2) (aref (x p) 2))  2)
						       (expt  (- (aref (x object) 3) (aref (x p) 3))  2)
						       )  -1.5))
					     0.0)))
		 ;;:pitch (rotate-4-vector (pitch p) (roll p) (* deltat (omega p)))
		 ;;:yaw (rotate-4-vector (yaw p) (roll p) (* deltat (omega p)))))
		 :pitch (rotate-4-vector (pitch p) (yaw p) (* deltat (omega p)))
		 :roll (rotate-4-vector (roll p) (yaw p) (* deltat (omega p)))))
