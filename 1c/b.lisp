;;; Google Code Jam 2019, round 1C
;;; Problem "Power Arrangers"
;;;
;;; Author:       Simon Leinen  <simon.leinen@gmail.com>
;;; Date created: 2019-05-04
;;;
;;; Solution approach:
;;;
;;; We tackle the "hard" variant of the problem, where we are only
;;; allowed 150 queries.  The basic idea is that we spend the first
;;; 119 queries to find out which of the first letters is missing.  We
;;; do so by asking for the first letter in each of the 119
;;; permutations.  In the 119 responses, every letter instead of one
;;; will occur 24 times.  The one that occurs only 23 times is the
;;; first letter of the missing permutation.
;;;
;;; (Note that at this point, a small optimization would be possible:
;;; As soon as we have four letters that have occurred 24 times, we
;;; don't need to query for any first letters anymore, because we know
;;; that the remaining indices MUST necessarily resolve to the missing
;;; letter.  This is a good example of an optimization that's probably
;;; not worth the effort; the small performance gain is not worth
;;; getting it wrong.  On the other hand, some programmers may find it
;;; inelegant to make queries when the result can be determined.)
;;;
;;; This leaves us with 23 remaining possible permutations, of which
;;; we know the first letter.  So we can make 23 queries for their
;;; respective second letters.  All second letters except one will
;;; occur 6 times.  The one will occur 5 times and is the second
;;; letter of the missing permutation.
;;;
;;; And so on... after 119 + 23 + 5 + 1 (= 148) queries, we should
;;; have excluded all permutations and know the one that's missing.
;;;
;;; At each "level" N (=1,2,3,4) of the algorithm, we want to query
;;; the Nth letter of each permutation that starts with the currently
;;; learned prefix that is common with the missing permutation.
;;;
;;; Handling the indices into the long vector is a bit tricky.  At
;;; each level, we keep a vector of indices of these letters of
;;; interest.  Initially, this is the vector [0,5,10,11,...,595], with
;;; 119 elements.  (We use zero-based indexing internally, adding 1
;;; when doing the queries.)  For the next level, we look at only the
;;; "interesting" permutations, and for each previous index i, keep
;;; i+1 if C_i was the "right" letter.  That might leave us with
;;; something like [1,11,26,...] if the first, third, and sixth
;;; permutation started with the "right" letter.
;;;
;;; The function MAKE-INDEX-MAP computes the next index map for each
;;; step.
;;;
;;; There is one special case that took me a while to understand: At
;;; the last level, we only have a single permutation remaining to
;;; query.  We ask for its fourth letter.  Above, we stated the rule
;;; to find out the next letter of the missing permutation in this
;;; form:
;;;
;;; "every letter instead of one will occur N times.  The one that
;;; occurs only N-1 times is the first letter of the missing
;;; permutation."
;;;
;;; This doesn't really apply at this level, where N=1.  One letter
;;; occurs N (1) times, but all other letters occur N-1 (0) times.  We
;;; which one is it? Fortunately we know that it cannot be one of the
;;; (3) letters that we already know! Of the remaining two, we have
;;; just excluded one, which leaves only one possibility.
;;;
;;; Finally, now that we know the first four letters, we add the
;;; remaining missing letter for the full missing permutation.

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
		(format o "~{~C~}~%" result)
		(finish-output o)
		(let ((response (read-line i)))
		  (assert (string= response "Y")))
		(return-from solve-case-1 result))))))))

(solve)
