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

(defun prime-to-char (cp-orig c0 cn)
  (let ((cp (make-array (list (+ (length cp-orig) 2)))))
    (dotimes (k (length cp-orig))
      (setf (aref cp (+ k 1)) (aref cp-orig k)))
    (setf (aref cp 0) (/ c0 (aref cp 1)))
    (setf (aref cp (+ (length cp-orig) 1)) (/ cn (aref cp (length cp-orig))))

    (let ((sorted-primes
	   (sort (remove-duplicates (copy-seq cp)) #'<))
	  (prime-to-char (make-hash-table :test #'eql))
	  (ch #\A))
      (dotimes (k (length sorted-primes) prime-to-char)
	(let ((prime (aref sorted-primes k)))
	  (setf (gethash prime prime-to-char)
		ch))
	(setq ch (code-char (+ (char-code ch) 1)))))))

(defun out-decoded-rle (ciph cp c-rl p->c)
  (multiple-value-bind (outs first)
      (out-rle-chunk (aref ciph 0) (aref c-rl 0) p->c :next (aref cp 0))
    (declare (ignore first))
    (format t "~A" outs))
  (let ((start (aref cp 0)) outs)
    (dotimes (k (length cp))
      (multiple-value-setq (outs start)
	(out-rle-chunk (aref ciph (+ k 1)) (aref c-rl (+ k 1)) p->c :start start))
      (format t "~A" outs))
    (let ((n (length ciph)))
      (multiple-value-setq (outs start)
	(out-rle-chunk (aref ciph (- n 1)) (aref c-rl (- n 1)) p->c :previous start)))))

(defun out-rle-chunk (ciph rl p->c &key next start previous)
  (if next
      ;; output sequence before the first known prime, back to front
      (let ((s '()))
	(dotimes (i rl)
	  (let ((x (/ ciph next)))
	    (push x s)
	    (setq next x)))
	(values (map 'string p->c s) next))
      (progn
	(when previous
	  (setq start (/ ciph previous)))
	(let ((s '()))
	  (dotimes (i rl)
	    (push start s)
	    (setq start (/ ciph start)))
	  (values (map 'string p->c (reverse s)) start)))))

(defun solve-case (caseno in)
  (let ((N (read in))
	(L (read in))
	(last nil)
	(ciph-rle '()))

    (labels
	()
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
	(let ((cp (clearprimes (apply #'vector (mapcar #'car ciph-rle)))))
	  (let ((prime-to-char (prime-to-char cp (aref ciph 0) (aref ciph (- (length ciph) 1)))))
	    (format t "Case #~D: "
		    (+ caseno 1))
	    (out-decoded-rle ciph cp c-rl
			     #'(lambda (x) (gethash x prime-to-char #\?)))
	    (format t "~%")))))))
