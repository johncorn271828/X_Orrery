#|
For drawing a heads-up display.
|#

(defun princ-to-screen (thing x y)
  (gl:color 1 1 1)
  (let* ((str (princ-to-string thing)))
    (loop for i from 0 to (- (length str) 1) do
	 (gl:window-pos (+ x (* i 9)) y 0)
	 (glut:bitmap-character glut:+bitmap-9-by-15+ (char-code (aref str i))))))


(defgeneric draw-hud (w))
(defmethod draw-hud ((w X_Orrery-gui))
  
  ;;speedometer relative to earth
  (princ-to-screen "v = " 10 10)
  (princ-to-screen (v (player *model*)) 50 10)
  
  ;;put labels on objects
  (loop for object in (objects *model*) do
       (let* ((screen-coords (multiple-value-list (glu:project (aref (x object) 1)
							       (aref (x object) 2)
							       (aref (x object) 3))))
	      (distance  (sqrt (+ (expt (aref (x object) 1) 2)
				  (expt (aref (x object) 2) 2)
				  (expt (aref (x object) 3) 2))))
	      (screenx (round (first screen-coords)))
	      (screeny (round (second screen-coords)))
	      ;;(screenz (third screen-coords))
	      (behind (> (third screen-coords) 1))
	      (drawx (if behind
			 nil
			 (cond ((< screenx 0)
				1)
			       ((> screenx (- (glut:game-mode-get :game-mode-width) (* (length (name object)) 9)))
				(- (glut:game-mode-get :game-mode-width) (* (length (name object)) 9)))
			       (t screenx))))
	      (drawy (if behind
			 nil
			 (cond ((< screeny 0)
				0)
			       ((> screeny (- (glut:game-mode-get :game-mode-height) 9))
				(- (glut:game-mode-get :game-mode-height) 9))
			       (t screeny)))))
	 
	 ;;;label planets, the sun, and other close stuff, plus ETAs
	 (if (and drawx
		  drawy
		  (not (equal (name object) "Starfield"))
		  (or (typep object 'sun)
		      (typep object 'planet)
		      (and (typep object 'moon) (< distance 10))
		      (< distance 1)))
	     (progn
	       (princ-to-screen (name object) drawx drawy)
	       (if (and (> drawx 100) 
			(< drawx (- (glut:game-mode-get :game-mode-width) 100)) 
			(> drawy 50) 
			(< drawy (- (glut:game-mode-get :game-mode-height) 50)))
		   (cond ((> distance 3600)
			  (princ-to-screen (concatenate 'string
							(princ-to-string (floor (round (/ distance 60)) 60))
							"h "
							(princ-to-string (mod (round (/ distance 60)) 60))
							"m")
					   (+ drawx 4) (- drawy 16)))
			 ((> distance 60)
			  (princ-to-screen (concatenate 'string
							(princ-to-string (round (/ distance 60)))
							"m")
					   (+ drawx 9)
					   (- drawy 16)))
			 ((> distance 1)
			  (princ-to-screen (concatenate 'string
							(princ-to-string (round distance))
							"s")
					   (+ drawx 9)
					   (- drawy 16)))
			 (t
			  nil)))))))

  ) ;end draw hud method
