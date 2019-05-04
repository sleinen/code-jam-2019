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
    (let ((result (solve-1 (list (cons '() progs)))))
      (if result
          (format t "Case #~D: ~{~A~}~%" (1+ caseno) result)
          (format t "Case #~D: IMPOSSIBLE~%" (1+ caseno))))))

(defun beatsp (a b)
  (case a ((#\R) (eql b #\S)) ((#\P) (eql b #\R)) ((#\S) (eql b #\P))))

(defun rotate (prog)
  (append (rest prog) (list (first prog))))

(defun solve-1 (prefs-and-progs)
  (let ((next-draw '()))
    (dolist (mymove '(#\R #\P #\S))
      (block move
        (let ((win nil)
              (draw (make-hash-table :test #'equal)))
          (dolist (pref-and-progs prefs-and-progs)
            (let ((pref (car pref-and-progs))
                  (progs (cdr pref-and-progs)))
              (dolist (advprog progs)
                (let ((advmove (first advprog)))
                  (cond ((beatsp advmove mymove)
                         (return-from move))
                        ((beatsp mymove advmove)
                         (setq win (cons (cons mymove pref) (rotate advprog))))
                        (t
                         (push (rotate advprog)
                               (gethash (cons mymove pref) draw '()))))))))
          (if (zerop (hash-table-count draw))
              (when win (return-from solve-1 (reverse (car win))))
              (maphash #'(lambda (pref progs)
                           (push (cons pref progs) next-draw))
                       draw)))))
    (if (endp next-draw)
        nil
        (solve-1 next-draw))))

(solve)
