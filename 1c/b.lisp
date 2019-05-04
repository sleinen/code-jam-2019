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
;;; At each level, we keep a list of indices of the letters of
;;; interest.  Initially, this is the list (0,5,10,11,...,595), with
;;; 119 elements.  (We use zero-based indexing internally, adding 1
;;; when doing the queries.)  As we query letters, we collect the
;;; index of the _next_ letter under each letter.  At the next level,
;;; we continue with the stored next-indices for the letter of
;;; interest.
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
;;;
;;; For no extra points, we generalized the algorithm to deal with
;;; permutations of arbitrary lengths N.  The program simply assumes
;;; that the permitted number of queries (f) is sufficient.

(defpackage :b (:use :common-lisp))

(in-package :b)

(defun solve-with-streams (i o)
  (let* ((ncase (read i))
         (f (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
        (let ((syms '(#\A #\B #\C #\D #\E)))
          (solve-case f i o syms (length syms)))))))

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

(defun fac (n) (fac1 n 1))
(defun fac1 (n r) (if (zerop n) r (fac1 (1- n) (* n r))))

(defun solve-case (f i o syms n)
  (let ((fac-1 (1- (fac n))))
    (let ((indices '()))
      (dotimes (a fac-1)
        (push (* a n) indices))
      (solve-case-1 f i o syms indices fac-1 n '()))))

(defun solve-case-1 (f i o syms indices count n result)
  (let ((next-count (1- (/ (1+ count) n)))
        (char->indices (make-hash-table)))
    (dolist (index indices)
      (format o "~D~%" (1+ index))
      (finish-output o)
      (let ((response (read-line i)))
        ;; (warn "~D -> ~D -> ~A" a index response)
        (unless (= (length response) 1)
          (error "Response: ~A" response))
        (push (1+ index) (gethash (aref response 0) char->indices '()))))
    (dolist (c syms)
      (when (and (= (length (gethash c char->indices)) next-count)
                 (not (member c result)))
        (if (> next-count 0)
            (solve-case-1
             f i o syms
             (gethash c char->indices)
             next-count
             (1- n)
             (cons c result))
            (progn
              (push c result)
              (dolist (c syms)
                (unless (member c result)
                  (push c result)))
              (setq result (nreverse result))
              (format o "~{~C~}~%" result)
              (finish-output o)
              (let ((response (read-line i)))
                (assert (string= response "Y")))
              (return-from solve-case-1 result)))))))

(solve)
