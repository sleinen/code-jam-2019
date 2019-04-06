;;; Google Code Jam 2019, Qualification Round, Problem 3: Cryptopangrams

(defun solve (&optional (in *standard-input*))
  (let ((ncases (read in)))
    (dotimes (caseno ncases)
      (solve-case caseno in))))

(defun clearprimes (ciph)
  (let ((cp (make-array (- (length ciph) 1))))
    (dotimes (k (- (length ciph) 1))
      (setf (aref cp k)
	    (gcd (aref ciph k) (aref ciph (+ k 1)))))
    cp))

(defun expand-rle (ciph c-rl cp)
  (let ((result
	 (expand-chunk (aref ciph 0) (aref c-rl 0) :next (aref cp 0))))
    (dotimes (i (length cp))
      (setq result
	    (append result
		    (expand-chunk (aref ciph (+ i 1)) (aref c-rl (+ i 1))
				  :start (aref cp i)))))
    (setq result
	  (append result
		  (expand-chunk (aref ciph (- (length ciph) 1))
				1
				:previous (car (last result)))))
    result))

(defun expand-chunk (ciph rl &key next start previous)
  (if next
      ;; generate sequence before the first known prime, back to front
      (let ((s '()))
	(dotimes (i rl)
	  (let ((x (/ ciph next)))
	    (push x s)
	    (setq next x)))
	s)
      (progn
	(when previous
	  (setq start (/ ciph previous)))
	(let ((s '()))
	  (dotimes (i rl)
	    (push start s)
	    (setq start (/ ciph start)))
	  (reverse s)))))

(defun prime-to-char (cp)
  (let ((sorted-primes
	 (sort (remove-duplicates (copy-seq cp)) #'<))
	(prime-to-char (make-hash-table :test #'eql))
	(ch #\A))
    (dotimes (k (length sorted-primes) prime-to-char)
      (let ((prime (elt sorted-primes k)))
	(setf (gethash prime prime-to-char)
	      ch))
      (setq ch (code-char (+ (char-code ch) 1))))))

(defun solve-case (caseno in)
  (let ((N (read in))
	(L (read in))
	(last nil)
	(ciph-rle '()))

    ;; We need to be careful about repeated symbols in the ciphertext.
    ;; Therefore, we read the symbols into a run-length encoded
    ;; sequence first.
    (dotimes (k L)
      (let ((x (read in)))
	(assert (<= x (* N N)))
	(if (and last (= last x))
	    (incf (cdr (first ciph-rle)))
	    (push (cons x 1) ciph-rle))
	(setq last x)))
    (setq ciph-rle (nreverse ciph-rle))

    (let ((ciph (apply #'vector (mapcar #'car ciph-rle)))
	  (c-rl (apply #'vector (mapcar #'cdr ciph-rle))))
      (let ((cp-rle (clearprimes (apply #'vector (mapcar #'car ciph-rle)))))
	(let ((cp (expand-rle ciph c-rl cp-rle)))
	  (let ((prime-to-char (prime-to-char cp)))
	    (format t "Case #~D: "
		    (+ caseno 1))
	    (dotimes (k (length cp))
	      (format t "~C" (gethash (elt cp k) prime-to-char #\?)))
	    (format t "~%")))))))
