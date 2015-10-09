#|
This is the top level output-handling file. Given the state of the opengl-cl documentation, it is difficult
to better explain what some of this code does without lots more research (to be completed).
|#

;;;Audio commented out until cross-platform testing.
;;(load "audio.lisp")

;;;This is the gui class and the display method acts as the main "read-eval-print" loop
(defclass X_Orrery-gui (glut:window)
  ((hud-text :accessor hud-text :initform "hello")
   (draw-list :accessor draw-list)
   (textures :accessor textures)
   (fov :accessor fov :initform 0.9) ;0.6109)  ;;field of view angle in radians
   (music-daemon :accessor music-daemon))
  (:default-initargs
   :game-mode 't 
   :mode '(:double :rgba :depth ) :title "X_Orrery.lisp"))

;;You have to pass this to glut even for fullscreen apps.
(defmethod glut:reshape ((w X_Orrery-gui) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective (* (fov w) (/ 180 pi))  ;vertical field of view angle in degrees
		   (/ width height)        ;aspect ratio
		   ;;znear=1e-4 and zfar=1e4 seem to work for everything but closest stuff
		   1e-4
		   1e4)                
  (gl:matrix-mode :modelview))

;;;Load meshes from .obj files.
(load "meshloader.lisp")

;;;Populate GL's display lists.
(load "displaylistpopulator.lisp")

;;;Load textures into GL's texture list.
(load "textureloader.lisp")

;;Called once when window is first displayed.
(defmethod glut:display-window :before ((w X_Orrery-gui))  

  (if (not (glut:game-mode-get :game-mode-possible))
      (progn
	(princln "Error: glut game mode not possible.")
	(exit)))

  ;;get the cursor started sanely
  (glut:set-cursor 101) ;101 is the value of GLUT_CURSOR_NONE.
  (glut:warp-pointer (/ (glut:game-mode-get :game-mode-width) 2) (/ (glut:game-mode-get :game-mode-height) 2))

  ;;this is necessary on linux
  (glut:set-key-repeat :key-repeat-off)

  ;;need to look up these functions and insert decent comments
  (gl:cull-face :back)
  (gl:depth-func :lequal)
  (gl:disable :dither)
  (gl:shade-model :smooth) ;previously :flat
  (gl:light-model :light-model-ambient #(0.1 0.1 0.1 1))  ;;turns ambient light down low
  (gl:depth-range 0 1)
  (gl:enable :light0 :lighting :cull-face :depth-test)  ;;can only have 8 of these

  ;;set up gl with the display lists and textures
  (populate-display-lists w)
  (load-textures w)
  
  ;;initialize camera angle
  (glu:look-at
   0 0 0
   (cos (phi (player *model*))) (sin (phi (player *model*))) (cos (theta (player *model*)))
   0 0 1))

;;;Load rendering code that calls draw lists, specifies materials, etc. 
(load "renderer.lisp")

;;;Code for drawing a heads-up-display
(load "hud.lisp")

;;;This is the main loop of the program!!!
(defmethod glut:display ((w X_Orrery-gui))
  (gl:clear :color-buffer :depth-buffer)
  ;;update state
  (setf *model* (get-new-state (copy-instance *model*) (copy-instance *controller*) (get-internal-real-time)))
  ;;update controller
  (update-controller)
  (gl:light :light0 :position (vector (aref (x (second (objects *model*))) 1) 
				      (aref (x (second (objects *model*))) 2) 
				      (aref (x (second (objects *model*))) 3) 1))
  ;;position camera
  (gl:load-identity)
  (glu:look-at
   0 0 0
   (cos (phi (player *model*))) (sin (phi (player *model*))) (cos (theta (player *model*)))
   0 0 1)
  ;;draw stuff
  (loop for object in (objects *model*) do ;eventually update to draw only relevant things
       (let ((render-cube-size (* 60000 (size object) (size object))))
	 (when (or (typep object 'sun)
		   (and (< (abs (aref (x object) 1)) render-cube-size)
			(< (abs (aref (x object) 2)) render-cube-size)
			(< (abs (aref (x object) 3)) render-cube-size)))
	   (render object)))
    (render (player *model*))
    (draw-hud w))
  (glut:swap-buffers))


;;;I am not entirely clear on the importance of this, but it seemed like there were race conditions or something
;;  whacked out going on with glut before I did this.
(defmethod glut:idle ((w X_Orrery-gui))
  (glut:post-redisplay))





