#|
This file loads meshes from .obj files for populating the gl display lists.
|#


;;this class keeps track of the vector, texture, normal indices of a polygon face as in a .obj file
(defclass face-indexer ()
  ((vertex-indices :accessor vertex-indices :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (texture-indices :accessor texture-indices :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (normal-indices :accessor normal-indices :initform (make-array 0 :fill-pointer 0 :adjustable t))))

;;this class represents a polygon mesh
(defclass mesh ()
  ((vertices :accessor vertices :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (textures-vertices :accessor texture-vertices :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (normals :accessor normals :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (faces :accessor faces :initform nil)))


(defun read-mesh-from-obj-file (filename scale)
  (let ((m (make-instance 'mesh))  ;mesh to be constructed
	(line-counter 0)
	(line-parts)
	(vert-parts)
	(a-face))
    ;;open file specified by filename
    (with-open-file (stream filename)
      ;;iterate over lines
      (loop for line = (read-line stream nil :eof) until (eq line :eof) do
	 ;;(debugvar "reading line number" line-counter)
	   (setf line-counter (+ line-counter 1))
	 ;;split the line on spaces
	   (setf line-parts (split-sequence:SPLIT-SEQUENCE #\Space line))
	 ;;handle v, vt, vn, and f lines separately
	   (cond ((equal (first line-parts) "v") ;vertex line found
		  (vector-push-extend (vector (* scale (parse-number:PARSE-REAL-NUMBER (second line-parts)))
					      (* scale (parse-number:PARSE-REAL-NUMBER (third line-parts)))
					      (* scale (parse-number:PARSE-REAL-NUMBER (fourth line-parts))))
				      (vertices m)))
		 ((equal (first line-parts) "vt") ;texture line found
		  (vector-push-extend (vector (parse-number:PARSE-REAL-NUMBER (second line-parts))
					      (parse-number:PARSE-REAL-NUMBER (third line-parts)))
				      (texture-vertices m)))
		 ((equal (first line-parts) "vn") ;normal vector found
		  (vector-push-extend (vector (parse-number:PARSE-REAL-NUMBER (second line-parts))
					      (parse-number:PARSE-REAL-NUMBER (third line-parts))
					      (parse-number:PARSE-REAL-NUMBER (fourth line-parts)))
				      (normals m)))
		 ((equal (first line-parts) "f") ;face found
		  (progn
		    (setf a-face (make-instance 'face-indexer))
		    (loop for item in (rest line-parts) do
			 (setf vert-parts (split-sequence:SPLIT-SEQUENCE #\/ item))
			 (vector-push-extend (parse-number:PARSE-NUMBER (first vert-parts))
					     (vertex-indices a-face))
			 (if (not (equal "" (second vert-parts)))
			     (vector-push-extend (parse-number:PARSE-NUMBER (second vert-parts))
						 (texture-indices a-face)))
			 (if (not (equal "" (third vert-parts)))
			     (vector-push-extend (parse-number:PARSE-NUMBER (third vert-parts))
						 (normal-indices a-face))))
		    (push a-face (faces m))))
		 (t nil))))
    m))


