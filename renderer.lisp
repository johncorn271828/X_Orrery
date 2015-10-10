#|
This file contains the code that draws objects, mostly by calling GL display lists and textures
as specified in displaylistpopulator.lisp and textureloader.lisp. 
Todo: Refactor huge cond black into a single statement with a display-attribute-wrapper object
for each renderable (or something like that).
|#

;;;Rendering functions
(defgeneric render (p))


(defmethod render ((rb rigid-body))
  (let* ((rollx (aref (roll rb) 1))
	 (rolly (aref (roll rb) 2))
	 (rollz (aref (roll rb) 3))
	 (pitchx (aref (pitch rb) 1))
	 (pitchy (aref (pitch rb) 2))
	 (pitchz (aref (pitch rb) 3))
	 (yawx (aref (yaw rb) 1))
	 (yawy (aref (yaw rb) 2))
	 (yawz (aref (yaw rb) 3))
	 (M    (vector rollx rolly rollz 0  pitchx pitchy pitchz 0 yawx yawy yawz 0 0 0 0 1))
	 (Minv (vector rollx pitchx yawx 0 rolly pitchy yawy 0 rollz pitchz yawz 0 0 0 0 1)))
    (gl:with-pushed-matrix
      ;;translate
      (gl:translate (aref (x rb) 1)
		    (aref (x rb) 2)
		    (aref (x rb) 3))
      ;;rotate
      (gl:mult-matrix M)
      ;;draw
      (cond ((and (typep rb 'celestial-body) (equal (name rb) "Starfield"))
	     (progn
	       (gl:material :front :emission #(1 1 1 1))
	       (gl:disable :cull-face)
	       (gl:depth-mask 0)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 0 (textures *view*)))
	       (gl:call-list (+ 0 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0) 
	       (gl:depth-mask 1)
	       (gl:enable :cull-face)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Sun"))
	     (progn
	       (gl:material :front :emission #(1 1 0 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 1 (textures *view*)))
	       (gl:call-list (+ 1 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Mercury"))
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 2 (textures *view*)))
	       (gl:call-list (+ 2 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Venus"))
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 3 (textures *view*)))
	       (gl:call-list (+ 3 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Earth"))
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 4 (textures *view*)))
	       (gl:call-list (+ 4 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Mars"))
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 5 (textures *view*)))
	       (gl:call-list (+ 5 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Jupiter"))
	     (progn
	       (gl:material :front :emission #(0.01 0.01 0.01 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 6 (textures *view*)))
	       (gl:rotate 90 0 0 1)
	       (gl:call-list (+ 6 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Saturn"))
	     (progn
	       (gl:material :front :emission #(0.01 0.01 0.01 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       ;;draw saturn
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 7 (textures *view*)))
	       (gl:call-list (+ 7 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0) 
	       ;;draw rings, top first
	       (gl:material :front :emission #(0.7 0.7 0.7 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:material :back :emission #(0.01 0.01 0.01 1))
	       (gl:material :back :ambient #(1 1 1 1))
	       (gl:material :back :diffuse #(1 1 1 1))
	       (gl:material :back :shininess 128)
	       (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 11 (textures *view*)))
	       (gl:call-list (+ 13 (draw-list *view*)))
	       ;;bottom of rings
	       (gl:rotate 180 1 0 0)
	       (gl:material :front :emission #(0.01 0.01 0.01 1))  ;;weird how this looks same?
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:material :back :emission #(0.01 0.01 0.01 1))
	       (gl:material :back :ambient #(1 1 1 1))
	       (gl:material :back :diffuse #(1 1 1 1))
	       (gl:material :back :shininess 128)
	       (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 11 (textures *view*)))
	       (gl:translate 0 0 0.001)
	       (gl:call-list (+ 13 (draw-list *view*)))
	       (gl:rotate 180 1 0 0)
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Uranus"))
	     (progn
	       (gl:material :front :emission #(0.01 0.01 0.01 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 8 (textures *view*)))
	       (gl:call-list (+ 8 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Neptune"))
	     (progn
	       (gl:material :front :emission #(0.01 0.01 0.01 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 9 (textures *view*)))
	       (gl:call-list (+ 9 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((and (typep rb 'celestial-body) (equal (name rb) "Moon"))
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 10 (textures *view*)))
	       (gl:call-list (+ 10 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
	    ((typep rb 'laboratory)
	     (progn
	       (gl:material :front :emission #(0.03 0.03 0.03 1))
	       (gl:material :front :ambient #(1 1 1 1))
	       (gl:material :front :diffuse #(1 1 1 1))
	       (gl:material :front :shininess 128)
	       (gl:enable :texture-2d)
	       (gl:bind-texture :texture-2d (nth 12 (textures *view*)))
	       (gl:call-list (+ 12 (draw-list *view*)))
	       (gl:bind-texture :texture-2d 0)))
      )
      ;;unrotate
      (gl:mult-matrix Minv)
      ;;untranslate
      (gl:translate (- (aref (x rb) 1))
		    (- (aref (x rb) 2))
		    (- (aref (x rb) 3))))))





(defmethod render ((p player))

  ;;get rid of depth testing and just make the player vessel big enough to be outside znear
  (gl:disable :depth-test)
  (gl:with-pushed-matrix

      
      (gl:material :front-and-back :emission #(0.2 0.2 0.2 1))
    (gl:material :front :ambient #(1 1 1 1))
    (gl:material :front :diffuse #(1 1 1 1))
    (gl:material :front :shininess 128)

    ;;a little crosshair to show direction
    (gl:with-primitives :lines
      (gl:vertex 1 -0.005 0)
      (gl:vertex 1 0.005 0))

    ;;load a wireframe
    (gl:translate 1.6 0 -5.2) ;-4.1)
    (gl:rotate 90 0 0 1)
    (gl:rotate 90 1 0 0)
    (gl:call-list (+ 11 (draw-list *view*)))
    )
  (gl:enable :depth-test)
  )

