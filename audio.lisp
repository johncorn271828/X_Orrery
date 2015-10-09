#|
This file contains code to spawn threads for audio events, including music. Not tested on windows.
|#

;;See the cl-openal github for info about how this works.
(defun wav-thread (file-name &optional (x 0.0) (y 0.0) (z 0.0))
  (let ((data nil)
	(format nil)
	(stereop nil)
	(bytes-per-sample)
	(size nil)
	(frequency nil)
	(duration nil)
	(data-info-list))
    (alut:with-init
      ;;load the song data
      (setf data-info-list (multiple-value-bind (d fo s fr) (alut:load-memory-from-file file-name) (list d fo s fr)))
      (setf data (first data-info-list))
      (setf format (second data-info-list))
      (setf stereop (if (or (equal format :stereo8)
			    (equal format :stereo16))
			t
			nil))
      (setf bytes-per-sample (if (or (equal format :stereo8)
				     (equal format :mono8))
				 1
				 2))
      (setf size (third data-info-list))
      (setf frequency (round (fourth data-info-list)))
      ;;(debugvar "data-info-list" data-info-list)
      (setf duration (/ size
			frequency
			(if stereop 2 1)
			bytes-per-sample)))
    
    (alc:with-device (device)
      (alc:with-context (context device)
	(alc:make-context-current context)
	(al:with-buffer (buffer)
	  (al:with-source (source)
	    (al:buffer-data buffer format data size frequency)
	    (al:source source :buffer buffer)
	    (al:source source :position (vector x y z))
	    (al:source source :velocity #(0 0 0))
	    (al:listener :position #(0 0 0))
	    (al:listener :orientation #(1 0 0
					0 0 1))
	    ;; Let the music play...
	    (al:source source :looping :false)
	    (al:source-play source)
	    (sleep duration)
	    (al:source-stop source)))))))


(defun run-music-daemon ()
  (sleep 10)
  (loop while t do
       (loop for song in (list "May18.1.1.wav" "May23.5.1.wav") do
	    (wav-thread song))))
	    

(defun play-wav (filename &optional (x 0.0) (y 0.0) (z 0.0))          ;coordinates only work for mono files
  (bordeaux-threads:make-thread (lambda () (wav-thread filename x y z))))
