(defpackage :gopher (:use :common-lisp))

(in-package :gopher)

(defparameter tasks
  '((7 1000000)
    1
    2
    20
    50
    100
    50000
    500000
    999998
    999999
    1000000))

(deftype windmill-blade-number () '(unsigned-byte 8))

(defun judge (&optional (i *standard-input*) (o *standard-output*))
  (let ((task-params (first tasks))
	(tasks (rest tasks)))
    (let ((n (first task-params))
	  (m (second task-params)))
      (format t "~D ~D ~D~%" (length tasks) n m)
      (finish-output o)
      (dolist (task tasks)
	(interact-task task n m i o)))))

(defun dig (task windmills)
  (let ((result (make-array (list (array-dimension windmills 0))
			    :initial-element 0
			    :element-type 'windmill-blade-number)))
    (dotimes (k task result)
      (let ((r (random (length windmills))))
	(setf (aref result r)
	      (mod (1+ (aref result r)) (aref windmills r)))))))

(defun interact-task (task n m i o)
  (let ((windmills (read-line i)))
    (if (find #\Space windmills)
	(let ((w (make-array (list 18)
			     :element-type 'windmill-blade-number)))
	  (with-input-from-string (in windmills)
	    (dotimes (k 18)
	      (setf (aref w k) (read in))))
	  (format t "~{~D~^ ~}~%" (coerce (dig task w) 'list))
	  (interact-task task (- n 1) m i o))
	(let ((guess (parse-integer windmills)))
	  (format t "~D~%" (if (= guess task) 1 -1))))))

(judge)
