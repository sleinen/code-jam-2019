(defpackage :b (:use :common-lisp))

(in-package :b)

(defun solve-with-streams (i o)
  (let* ((ncase (read i))
	 (f (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
	(solve-case f i o)))))

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

(defun solve-case (f i o)
  (let ((index-map (make-array (list 119))))
    (dotimes (a 119)
      (setf (aref index-map a) (* a 5)))
    (solve-case-1 f i o index-map '(119 23 5 1 0) '())))

(defun make-index-map (old-im s c count)
  (let ((new (make-array (list count)))
	(new-i 0))
    (dotimes (old-i (length s) new)
      (when (eql (aref s old-i) c)
	(setf (aref new new-i) (+ (aref old-im old-i) 1))
	(incf new-i)))))

(defun solve-case-1 (f i o index-map counts result)
  (let ((count (first counts))
	(next-count (second counts)))
    (let ((s (make-array (list count))))
      (dotimes (a count)
	(let ((index (aref index-map a)))
	  (format o "~D~%" (1+ index))
	  (finish-output o)
	  (let ((response (read-line i)))
	    ; (warn "~D -> ~D -> ~A" a index response)
	    (unless (= (length response) 1)
	      (error "Response: ~A" response))
	    (setf (aref s a) (aref response 0)))))
      (dolist (c '(#\A #\B #\C #\D #\E))
	(when (and (= (count c s) next-count)
		   (not (member c result)))
	  (warn "Found letter ~C" c)
	  (if (rest (rest counts))
	      (solve-case-1
	       f i o
	       (make-index-map index-map s c next-count)
	       (rest counts)
	       (cons c result))
	      (progn
		(push c result)
		(dolist (c '(#\A #\B #\C #\D #\E))
		  (unless (member c result)
		    (push c result)))
		(setq result (nreverse result))
		(warn "found result: ~{~C~}" result)
		(format o "~{~C~}~%" result)
		(finish-output o)
		(let ((response (read-line i)))
		  (assert (string= response "Y")))
		(return-from solve-case-1 result))))))))

(solve)
