#|
Without calling image processing libraries, you can read .rgb files into OpenGL's texture memory.
|#

(defgeneric load-textures (w))
(defmethod load-textures ((w X_Orrery-gui))

  ;;tell gl to start a list of N textures
  (setf (textures w) (gl:gen-textures 30))
  
  (let ((texture-file-stream)
	(texture-file-data))

    ;;starfield 0
    (setf texture-file-stream (open "textures/starfield.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 0 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 4096 4096  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)

    ;;Sun 1
    (setf texture-file-stream (open "textures/sun.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 1 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)

    
    ;;Mercury 2
    (setf texture-file-stream (open "textures/mercury.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 2 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 2048 2048  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Venus 3
    (setf texture-file-stream (open "textures/venus.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 3 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Earth 4
    (setf texture-file-stream (open "textures/earth.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 4 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 2048 2048  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)

    ;;Mars 5
    (setf texture-file-stream (open "textures/mars.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 5 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 2048 2048 :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Jupiter 6
    (setf texture-file-stream (open "textures/jupiter.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 6 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 2048 2048 :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Saturn 7
    (setf texture-file-stream (open "textures/saturn.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 7 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024 :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Uranus 8
    (setf texture-file-stream (open "textures/uranus.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 8 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024 :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;Neptune 9
    (setf texture-file-stream (open "textures/neptune.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 9 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024 :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    ;;moon 10
    (setf texture-file-stream (open "textures/moon.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 10 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 2048 2048  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)

    ;;saturn's rings 11
     (setf texture-file-stream (open "textures/saturn-ring.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 11 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 512 512  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)

    ;;metal1 texture 12
    (setf texture-file-stream (open "textures/metal1.rgb" :direction :input :element-type '(unsigned-byte 8)))
    (setf texture-file-data (make-array (file-length texture-file-stream) :element-type '(unsigned-byte 8)))
    (read-sequence texture-file-data texture-file-stream)
    (close texture-file-stream)
    (gl:bind-texture :texture-2d (nth 12 (textures w)))
    (glu:build-2d-mipmaps :texture-2d 3 1024 1024  :rgb :unsigned-byte texture-file-data)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    
    
    )
  )
