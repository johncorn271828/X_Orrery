#|
Using GL's display lists allows better performance.
|#

(defgeneric populate-display-lists (w))
(defmethod populate-display-lists ((w X_Orrery-gui))

  (let ((sphere-ring-count 100))
    
    ;;generate display lists.
    ;;currently starfield sun, 8 planets, 2 satellites, player
    (setf (draw-list w) (gl:gen-lists 30))

    ;;starfield 0
    (gl:color 0 0 0)
    (gl:with-new-list ((+ 0 (draw-list w)) :compile)
      ;;bottom
      (let ((starfield-size 100000))
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex (- starfield-size) (- starfield-size) (- starfield-size))
	  (gl:tex-coord 0 1)
	  (gl:vertex (- starfield-size) starfield-size (- starfield-size))
	  (gl:tex-coord 1 1)
	  (gl:vertex starfield-size starfield-size (- starfield-size))
	  (gl:tex-coord 1 0)
	  (gl:vertex starfield-size (- starfield-size) (- starfield-size)))
	;;top
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex (- starfield-size) (- starfield-size) starfield-size)
	  (gl:tex-coord 0 1)
	  (gl:vertex (- starfield-size) starfield-size starfield-size)
	  (gl:tex-coord 1 1)
	  (gl:vertex starfield-size starfield-size starfield-size)
	  (gl:tex-coord 1 0)
	  (gl:vertex starfield-size (- starfield-size) starfield-size))
	;;left
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex (- starfield-size) starfield-size (- starfield-size))
	  (gl:tex-coord 0 1)
	  (gl:vertex (- starfield-size) starfield-size starfield-size)
	  (gl:tex-coord 1 1)
	  (gl:vertex starfield-size starfield-size starfield-size)
	  (gl:tex-coord 1 0)
	  (gl:vertex starfield-size starfield-size (- starfield-size)))
	;;right
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex (- starfield-size) (- starfield-size) (- starfield-size))
	  (gl:tex-coord 0 1)
	  (gl:vertex (- starfield-size) (- starfield-size) starfield-size)
	  (gl:tex-coord 1 1)
	  (gl:vertex starfield-size (- starfield-size) starfield-size)
	  (gl:tex-coord 1 0)
	  (gl:vertex starfield-size (- starfield-size) (- starfield-size)))
	;;front
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex starfield-size (- starfield-size) (- starfield-size))
	  (gl:tex-coord 0 1)
	  (gl:vertex starfield-size (- starfield-size) starfield-size)
	  (gl:tex-coord 1 1)
	  (gl:vertex starfield-size starfield-size starfield-size)
	  (gl:tex-coord 1 0)
	  (gl:vertex starfield-size starfield-size (- starfield-size)))
	;;back
	(gl:with-primitives :quads
	  (gl:tex-coord 0 0)
	  (gl:vertex (- starfield-size) (- starfield-size) (- starfield-size))
	  (gl:tex-coord 0 1)
	  (gl:vertex (- starfield-size) (- starfield-size) starfield-size)
	  (gl:tex-coord 1 1)
	  (gl:vertex (- starfield-size) starfield-size starfield-size)
	  (gl:tex-coord 1 0)
	  (gl:vertex (- starfield-size) starfield-size (- starfield-size)))))

    ;;Sun 1
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 1 (draw-list w)) :compile)
	(glu:sphere qo sun-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;Mercury 2
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 2 (draw-list w)) :compile)
	(glu:sphere qo mercury-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;Venus 3
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 3 (draw-list w)) :compile)
	(glu:sphere qo venus-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    

    ;;Earth 4
    (let ((earth-quadric-obj))
      (setf earth-quadric-obj (glu:new-quadric))
      (glu:quadric-draw-style earth-quadric-obj :fill)
      (glu:quadric-texture earth-quadric-obj 1)
      (glu:quadric-normals earth-quadric-obj :smooth)
      (gl:with-new-list ((+ 4 (draw-list w)) :compile)  
	(glu:sphere earth-quadric-obj earth-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric earth-quadric-obj))

    ;;Mars 5
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 5 (draw-list w)) :compile)
	(glu:sphere qo mars-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;Jupiter 6
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 6 (draw-list w)) :compile)
	(glu:sphere qo jupiter-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;Saturn 7 and 13
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 7 (draw-list w)) :compile)
	(glu:sphere qo saturn-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    ;;now the rings
    (gl:with-new-list ((+ 13 (draw-list w)) :compile)
      (loop for i from 0 to 40 do
	   (gl:with-primitives :quads
	     (gl:tex-coord 0 0)
	     (gl:vertex (* saturn-inner-ring-radius (cos (* i (/ (* 2 pi) 40))))
			(* saturn-inner-ring-radius (sin (* i (/ (* 2 pi) 40))))
			0)
	     (gl:tex-coord 0 1)
	     (gl:vertex (* saturn-outer-ring-radius (cos (* i (/ (* 2 pi) 40))))
			(* saturn-outer-ring-radius (sin (* i (/ (* 2 pi) 40))))
			0)
	     (gl:tex-coord 1 1)
	     (gl:vertex (* saturn-outer-ring-radius (cos (* (+ i 1) (/ (* 2 pi) 40))))
			(* saturn-outer-ring-radius (sin (* (+ i 1) (/ (* 2 pi) 40))))
			0)
	     (gl:tex-coord 1 0)
	     (gl:vertex (* saturn-inner-ring-radius (cos (* (+ i 1) (/ (* 2 pi) 40))))
			(* saturn-inner-ring-radius (sin (* (+ i 1) (/ (* 2 pi) 40))))
			0))))
    
    
    ;;Uranus 8
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 8 (draw-list w)) :compile)
	(glu:sphere qo uranus-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;Neptune 9  
    (let ((qo (glu:new-quadric)))
      (glu:quadric-draw-style qo :fill)
      (glu:quadric-texture qo 1)
      (glu:quadric-normals qo :smooth)
      (gl:with-new-list ((+ 9 (draw-list w)) :compile)
	(glu:sphere qo neptune-radius sphere-ring-count sphere-ring-count))
      (glu:delete-quadric qo))
    
    ;;moon 10
    (let ((moon-quadric-obj))
      (setf moon-quadric-obj (glu:new-quadric))
      (glu:quadric-draw-style moon-quadric-obj :fill)
      (glu:quadric-texture moon-quadric-obj 1)
      (glu:quadric-normals moon-quadric-obj :smooth)
      (gl:with-new-list ((+ 10 (draw-list w)) :compile)
	(glu:sphere moon-quadric-obj moon-radius sphere-ring-count sphere-ring-count)
	)
      (glu:delete-quadric moon-quadric-obj)
      )

    ;;player 11
    (gl:with-new-list ((+ 11 (draw-list w)) :compile)
      (let ((iss-mesh (read-mesh-from-obj-file "meshes/player.obj" 1))
	    (normal-index))
	(loop for face in (faces iss-mesh) do
	     (gl:with-primitives :line-loop
	       (setf normal-index (- (aref (normal-indices face) 0) 1))
	       ;;(debugvar "normal index" normal-index)
	       (gl:normal (aref (aref (normals iss-mesh) normal-index) 0)
			  (aref (aref (normals iss-mesh) normal-index) 1)
			  (aref (aref (normals iss-mesh) normal-index) 2))
	       (loop for i across (vertex-indices face) do
		    (gl:vertex (aref (aref (vertices iss-mesh) (- i 1)) 0)
			       (aref (aref (vertices iss-mesh) (- i 1)) 1)
			       (aref (aref (vertices iss-mesh) (- i 1)) 2)))))))
    

    
    ;;laboratory 12
    (gl:with-new-list ((+ 12 (draw-list w)) :compile)
      (let ((iss-mesh (read-mesh-from-obj-file "meshes/antenna.obj" (* 10000 meter)))
	    (normal-index))
	(loop for face in (faces iss-mesh) do
	     (if (= (length (vertex-indices face)) 4)
		 (gl:with-primitives :quads
		   (setf normal-index (- (aref (normal-indices face) 0) 1))
		   ;;(debugvar "normal index" normal-index)
		   (gl:normal (aref (aref (normals iss-mesh) normal-index) 0)
			      (aref (aref (normals iss-mesh) normal-index) 1)
			      (aref (aref (normals iss-mesh) normal-index) 2))
		   (gl:tex-coord 0 0)
		   (gl:vertex (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 0) 1)) 0)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 0) 1)) 1)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 0) 1)) 2))
		   (gl:tex-coord 0 1)
		   (gl:vertex (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 1) 1)) 0)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 1) 1)) 1)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 1) 1)) 2))
		   (gl:tex-coord 1 1)
		   (gl:vertex (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 2) 1)) 0)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 2) 1)) 1)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 2) 1)) 2))
		   (gl:tex-coord 1 0)
		   (gl:vertex (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 3) 1)) 0)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 3) 1)) 1)
			      (aref (aref (vertices iss-mesh) (- (aref (vertex-indices face) 3) 1)) 2)))))))

  

    ;;Saturn's rings are #13. see above


    ) ;;end let


  
  ) ;end defmethod

