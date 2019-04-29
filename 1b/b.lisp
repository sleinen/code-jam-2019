(defpackage :b (:use :common-lisp))

(in-package :b)

(defun solve-with-streams (i o)
  (let* ((ncase (read i))
	 (w (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
	(solve-case w i o)))))

(defun solve (&optional setno)
  (if setno
      (let ((p (sb-ext:run-program "/usr/local/bin/python3" (list "testing_tool.py" (format nil "~D" setno))
				   :input :stream :output :stream :wait nil)))
	(assert p)
	(unwind-protect
	     (solve-with-streams
	      (sb-ext:process-output p)
	      (sb-ext:process-input p))
	  (sb-ext:process-close p)))
      (solve-with-streams *standard-input* *standard-output*)))

(defstruct (well)
  (w)
  (ht (make-hash-table :test 'eql))
  (i)
  (o))

(defun ask-oracle (well n)
  (or (gethash n (well-ht well))
      (progn
	(format (well-o well) "~D~%" n)
	(finish-output (well-o well))
	(let ((result (read (well-i well))))
	  (warn "At the end of day ~D, Odin had ~D (~63,'0B) rings."
		n result result)
	  (setf (gethash n (well-ht well)) result)))))

(defun solve-case (w i o)
  (let ((well (make-well :w w :i i :o o)))
    (solve-case-1 i o well)))

(defun solve-case-1 (i o well)
  (let* ((d1 56)
	 (d1-rings (ask-oracle well d1)))
      (let* ((rest1 d1-rings)
	     (r1 (ash rest1 (- d1))))
	(let ((rest1 (- d1-rings (ash r1 d1))))
	  (let ((r2 (ash rest1 (- (floor d1 2)))))
	    (let ((rest1 (- rest1 (ash r2 (floor d1 2)))))
	      (let* ((d2 224)
		     (d2-rings (ask-oracle well d2)))
		(let* ((rest2 d2-rings)
		       (r4 (ash rest2 (- (floor d2 4)))))
		  (let* ((rest2 (- rest2 (ash r4 (floor d2 4))))
			 (r5 (ash rest2 (- (floor d2 5)))))
		    (let* ((rest2 (- rest2 (ash r5 (floor d2 5))))
			   (r6 (ash rest2 (- (floor d2 6)))))
		      (let* ((rest1 (- rest1 (+ (ash r4 (floor d1 4))
						(ash r5 (floor d1 5))
						(ash r6 (floor d1 6)))))
			     (r3 (ash rest1 (- (floor d1 3)))))
			(format o "~D ~D ~D ~D ~D ~D~%" r1 r2 r3 r4 r5 r6)
			(finish-output o)
			(let ((result (read i)))
			  (ecase result
			    ((-1) (warn "We lose"))
			    ((1) (warn "We win!"))))
			(return-from solve-case-1))))))))))))

(solve)
