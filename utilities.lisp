#|
This file has a few general-use functions.
|#

;;for purposes of immutability, this function produces a copy of an object with optional changed slots.
;;see http://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
	   (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
	(when (slot-boundp object slot-name)
	  (setf (slot-value copy slot-name)
		(slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defun princln (x)
  (princ x)
  (fresh-line))

(defun debugvar (label x)
  (princ label)
  (princ " = ")
  (princln x))

;;Form Paul Graham's book page 110
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))




