#|
This file handles keyboard and mouse inputs using glut. To allow more nuanced controls, some
state logic is passed from one instance of a controller object to the next. I'm not sure whether
these variables should be part of the model object.
|#

;;;Controller object for maintaining duration of key presses, mouse position, and mouse events.
(defclass controller ()
  ((wtf :initform (glut:init))
   (key-down-time-table :accessor key-down-time-table  :initform nil)
   (roll-rate :accessor roll-rate :initform 0)
   (pitch-rate :accessor pitch-rate :initform 0)
   (yaw-rate :accessor yaw-rate :initform 0)
   (mouse-events :accessor mouse-events :initform nil)
   (mouse-x :accessor mouse-x :initform (/ (glut:game-mode-get :game-mode-width) 2))
   (mouse-y :accessor mouse-y :initform (/ (glut:game-mode-get :game-mode-height) 2))
   (mouse-deltax :accessor mouse-deltax :initform 0)
   (mouse-deltay :accessor mouse-deltay :initform 0)))

;;;Declare some functions to pass to glut for input event handling.
;;Keyboard button events
(defmethod glut:keyboard ((w glut:window) key x y)
  (declare (ignore x y))
  (when (eq key #\Esc)
    (progn
      (princln "Exit requested...")
      ;;(bordeaux-threads:destroy-thread music-daemon) ;not sure if this is necessary
      (glut:destroy-current-window)))
  ;;(when (eq key #\f)  (play-wav "fart.wav" 0 -1 0))
  (unless (assoc key (key-down-time-table *controller*))
    (push (cons key (get-internal-real-time)) (key-down-time-table *controller*))))

(defmethod glut:keyboard-up ((w glut:window) key x y)
  (declare (ignore x y))
  (let ((entry (assoc key (key-down-time-table *controller*))))
    (when entry
      (setf (key-down-time-table *controller*) (remove entry (key-down-time-table *controller*))))))

;;Mouse motion events
(defmethod glut:passive-motion ((w glut:window) x y)
  (let ((deltax (- x (mouse-x *controller*)))
	(deltay (- y (mouse-y *controller*))))
    (if (and (< deltax 40) (< deltay 40))  ;set a limit on deltax to avoid the jumping
	(progn
	  (setf (mouse-deltax *controller*) deltax)
	  (setf (mouse-deltay *controller*) deltay))))
  ;;always warping pointer back to center causes a glut bug to act up
  (if (or (< x 100) (> x (- (glut:game-mode-get :game-mode-width) 100)) 
	    (< y 100) (> y (- (glut:game-mode-get :game-mode-height) 100)))
      (progn
	(glut:warp-pointer (/ (glut:game-mode-get :game-mode-width) 2) 
			   (/ (glut:game-mode-get :game-mode-height) 2))
	(setf (mouse-x *controller*) (/ (glut:game-mode-get :game-mode-width) 2))
	(setf (mouse-y *controller*) (/ (glut:game-mode-get :game-mode-height) 2)))
      (progn
	(setf (mouse-x *controller*) x)
	(setf (mouse-y *controller*) y))))

;;Mouse button events
(defmethod glut:mouse ((w glut:window) button state x y)
  (cond ((and (eq state :down) (eq button :wheel-up) (member :active-shift (glut:get-modifiers)))
	 (push 'shift-wheel-up (mouse-events *controller*)))
	((and (eq state :down) (eq button :wheel-up) (member :active-ctrl (glut:get-modifiers)))
	 (push 'control-wheel-up (mouse-events *controller*)))
	((and (eq state :down) (eq button :wheel-up) (not (member :active-shift (glut:get-modifiers))))
	 (push 'wheel-up (mouse-events *controller*)))
	((and (eq state :down) (eq button :wheel-down) (member :active-shift (glut:get-modifiers)))
	 (push 'shift-wheel-down (mouse-events *controller*)))
	((and (eq state :down) (eq button :wheel-down) (member :active-ctrl (glut:get-modifiers)))
	 (push 'control-wheel-down (mouse-events *controller*)))
	((and (eq state :down) (eq button :wheel-down) (not (member :active-shift (glut:get-modifiers))))
	 (push 'wheel-down (mouse-events *controller*)))))

;;;This function will get called after every state update to update the *controller* global state variable.
(defun update-controller ()
  ;;reset mouse
  (setf (mouse-deltax *controller*) 0)
  (setf (mouse-deltay *controller*) 0)
  (setf (mouse-events *controller*) nil)  ;change this line to allow holding down mouse buttons
  ;;The following is intended to smooth out the rotation controls. A better solution is probably needed
  ;;in subsequent versions.
  (let ((roll-sensitivity (/ pi 30 1000))
	(pitch-sensitivity (/ pi 200 1000))
	(yaw-sensitivity (/ pi 200 1000)))
    (cond ((and (assoc #\a (key-down-time-table *controller*))
		(assoc #\d (key-down-time-table *controller*)))
	   nil)
	  ((and (assoc #\a (key-down-time-table *controller*))
		(not  (assoc #\d (key-down-time-table *controller*))))
	   (setf (roll-rate *controller*)
		 (* +1 
		    roll-sensitivity 
		    (min 2000 (- (get-internal-real-time)
				 (cdr (assoc #\a (key-down-time-table *controller*))))))))
	  ((and (assoc #\d (key-down-time-table *controller*))
		(not  (assoc #\a (key-down-time-table *controller*))))
	   (setf (roll-rate *controller*)
		 (* -1 
		    roll-sensitivity 
		    (min 2000 (- (get-internal-real-time)
				 (cdr (assoc #\d (key-down-time-table *controller*))))))))
	  (t
	   (setf (roll-rate *controller*) 0)))
    (cond ((and (assoc #\w (key-down-time-table *controller*))
		(assoc #\s (key-down-time-table *controller*)))
	   nil)
	  ((and (assoc #\w (key-down-time-table *controller*))
		(not (assoc #\s (key-down-time-table *controller*))))
	   (setf (pitch-rate *controller*)
		 (* -1 
		    pitch-sensitivity 
		    (min 1500 (- (get-internal-real-time)
				 (cdr (assoc #\w (key-down-time-table *controller*))))))))
	  ((and (assoc #\s (key-down-time-table *controller*))
		(not (assoc #\w (key-down-time-table *controller*))))
	   (setf (pitch-rate *controller*)
		 (* +1 
		    pitch-sensitivity 
		    (min 1500 (- (get-internal-real-time)
				 (cdr (assoc #\s (key-down-time-table *controller*))))))))
	  (t
	    (setf (pitch-rate *controller*) 0)))
    (cond ((and (assoc #\q (key-down-time-table *controller*))
		(assoc #\e (key-down-time-table *controller*)))
	   nil)
	  ((and (assoc #\q (key-down-time-table *controller*))
		(not (assoc #\e (key-down-time-table *controller*))))
	   (setf (yaw-rate *controller*)
		 (* -1 
		    yaw-sensitivity 
		    (min 2000 (- (get-internal-real-time)
				 (cdr (assoc #\q (key-down-time-table *controller*))))))))
	  ((and (assoc #\e (key-down-time-table *controller*))
		(not (assoc #\q (key-down-time-table *controller*))))
	   (setf (yaw-rate *controller*)
		 (* +1 
		    yaw-sensitivity (min 2000 (- (get-internal-real-time)
						 (cdr (assoc #\e (key-down-time-table *controller*))))))))
	  (t
	    (setf (yaw-rate *controller*) 0)))))



