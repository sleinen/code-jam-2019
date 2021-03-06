(defpackage :a (:use :common-lisp))

;; easy: 1 <= A <= 7   ; 1 <= Ci <= 5
;; hard: 1 <= A <= 255 ; 1 <= Ci <= 500

(in-package :a)

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in)) (solve-case caseno in)))

(defun solve-case (caseno in)
  (let* ((n (read in))
         (progs '()))
    (dotimes (i n)
      (let* ((ptext (read-line in))
             (prog '()))
        (dotimes (k (length ptext))
          (push (aref ptext k) prog))
        (push (nreverse prog) progs)))
    (let ((result (solve-1 n '() progs)))
      (if result
          (format t "Case #~D: ~{~A~}~%" (1+ caseno) result)
          (format t "Case #~D: IMPOSSIBLE~%" (1+ caseno))))))

(defun beater (a)
  (case a ((#\S) #\R) ((#\R) #\P) ((#\P) #\S)))

(defun beatsp (a b)
  (eql a (beater b)))

(defun rotate (prog)
  (append (rest prog) (list (first prog))))

(defun solve-1 (n pref progs)
  (and (>= n 0)
       (let ((first-set (remove-duplicates (mapcar #'first progs))))
	 (case (length first-set)
	   ((3) nil)
	   ((1) (reverse (cons (beater (first first-set)) pref)))
	   ((2)
	    (let ((feasible
		   (first (intersection first-set (mapcar #'beater first-set)))))
              (solve-1
	       (1- n)
	       (cons feasible pref)
	       (mapcar #'rotate
		       (remove feasible progs :test-not #'eql :key #'first)))))))))

(solve)
