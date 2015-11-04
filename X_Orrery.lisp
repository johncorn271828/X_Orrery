#|
This is the main file for the X_Orrery prototype, a solar system simulator
and planetarium application. This is basically my playground from teaching 
myself OpenGL. 

I have attempted to design this with the most vanilla possible main loop, and
being purely functional when the libraries let me. Also, not being too clever.
It's a little bit cowboy code, but I'm happy to respond to inquiries about how
I was thinking.

To run this, you need sbcl, freeglut (freeglut3-dev in debian), and 
freealut (libalut-dev in debian). I use "sbcl --load". This will look for a
quicklisp installation, and install quicklisp to the working directory 
otherwise.

The next version is on the way and will have much better everything :)

By John Corn

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

|#

;;;Load other people's code using quicklisp: https://www.quicklisp.org/beta/
(load "quicklisp.lisp")
(unless (probe-file "~/quicklisp/setup.lisp")
  (quicklisp-quickstart:install))
(load "~/quicklisp/setup.lisp") 
;;Common Common Lisp utilities
(ql:quickload "split-sequence")
(ql:quickload "parse-number")
(ql:quickload "bordeaux-threads")
;;Graphics via OpenGL bindings.
(ql:quickload "cl-opengl")
(ql:quickload "cl-glu")
(ql:quickload "cl-glut")
;;Audio via openal..
(ql:quickload "cl-openal")
(ql:quickload "cl-alc")
(ql:quickload "cl-alut")


;;globals for library-imposed use of observer pattern. Maybe this could be 
;;rewired with FRP?
(defparameter *model* nil)
(defparameter *view* nil)
(defparameter *controller* nil)

;;;Load my code
(load "utilities.lisp")  ;Generic stuff not specific to this program.
(load "controller.lisp")
(load "model.lisp")
(load "view.lisp")

;;Persistence is to-do
(defun saved-state-on-disk-p () nil)
(defun get-state-from-disk () nil)
;;(defun save-to-disk (old-state) nil)

(defun main ()
  (princln "Welcome to X_Orrery. Loading...")
  ;;This is needed here to pass screen dimensions to routines called before showing the window
  (glut:init)

  
  (princ "Controls... ")
  (setf *controller* (make-instance 'controller))
  (princln "done.")

  (princ "Initializing/recovering state... ")
  (setf *model* (if (saved-state-on-disk-p)
		      (progn
			(princln "found saved state.")
			(get-state-from-disk))
		      (progn
			(princln "starting anew.")
			(make-instance 'X_Orrery-state))))

  (princ "Starting graphics engine...")
  (setf *view* (make-instance 'X_Orrery-gui))
  (princln "done.")
    
  ;;Music
  (princ "Starting music daemon... ")
  (setf (music-daemon *view*) (bordeaux-threads:make-thread (lambda () (run-music-daemon))))
  (princln "done.")
  
  
  ;;Main loop is in the glut window methods. Ugly but fixed in upcoming Haskell version.
  (princln "Attempting to display window...")
  (glut:display-window *view*)

  ;;persist after exit
  ;;(princln "Saving state to disk...")
  ;;(save-to-disk *model*)

  (princln "exiting normally."))

;;Run without saving an executable file.
(main) (exit)

;;;This command instructs the sbcl compiler to write an executable. Problematic in windows.
;;(sb-ext:save-lisp-and-die "X_Orrery.bin" :executable t :toplevel 'main)
