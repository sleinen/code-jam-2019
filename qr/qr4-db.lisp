(defun solve-with-streams (i o)
  (let ((ncase (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
	(let* ((n (read i))
	       (b (read i))
	       (f (read i)))
	  (solve-case n b f i o))))))

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

(defun iota (n)
  (iota-1 n '()))

(defun iota-1 (n set)
  (if (zerop n) set (iota-1 (- n 1) (cons (- n 1) set))))

(defun solve-case (n b f in out)
  (setq f (min f (integer-length n)))
  (let ((attempts (make-array (list f)))
	(responses (make-array (list f))))
    ;; (assert (<= (integer-length n) f))
    (dotimes (i f)
      (setf (aref attempts i) (make-array (list n) :element-type 'bit)))
    (dotimes (k n)
      (dotimes (i f)
	(setf (aref (aref attempts i) k) (ldb (byte 1 i) k))))
    (dotimes (i f)
      (let ((attempt (aref attempts i)))
	(format out "~{~D~}~%" (coerce attempt 'list))
	(finish-output out)
	(let ((response (read-line in)))
	  (assert (= (length response) (- n b)))
	  (let ((response-bits (make-array (list (- n b)) :element-type 'bit)))
	    (dotimes (k (- n b))
	      (setf (aref response-bits k)
		    (ecase (aref response k)
		      ((#\0) 0)
		      ((#\1) 1))))
	    (setf (aref responses i)
		  response-bits)))))

    (labels ((matchp (a ai b bi f)
	       (dotimes (r f t)
		 (unless (= (aref (aref a r) ai)
			    (aref (aref b r) bi))
		   (return nil)))))

      (let ((broken
	     (do ((abit 0 (+ abit 1))
		  (rbit 0)
		  (broken '()))
		 ((>= abit n) (nreverse broken))
	       (if (>= rbit (- n b))
		   (pushnew abit broken)
		   (if (matchp attempts abit responses rbit f)
		       (incf rbit)
		       (pushnew abit broken))))))

	(format out "~{~D~^ ~}~%" broken)
	(finish-output out)
	(let ((verdict (read in)))
	  (when (= verdict -1)
	    (warn "lost verdict")
	    (throw 'solve-exit nil)))))))

(solve)
