(define (search p d)
  (match p p '() '() d '()))

;; Search p0 against pre0+d, 
;; with (p,pre) as our current positions, respectively.
;; Invariant: (or (null? d) (not (memv (car d) neg)))
(define (match p0 p pre pre0 d neg)
  (if (null? p) 
      #t
      (if (null? pre)
	  (if (null? d)
	      #f
	      (if (memv (car p) neg)
		  (restart p0 pre0 d neg)
		  (if (eqv? (car p) (car d))
		      (match p0 (cdr p) '() (snoc pre0 (car p)) (cdr d) '())
		      (restart p0 pre0 d (cons (car p) neg)))))
	  (if (eqv? (car p) (car pre))
	      (match p0 (cdr p) (cdr pre) pre0 d neg)
	      (match p0 p0 (cdr pre0) (cdr pre0) d neg)))))

(define (restart p0 pre0 d neg)
  (if (null? pre0)
      (search p0 (cdr d))
      (match p0 p0 (cdr pre0) (cdr pre0) d neg)))

(define (snoc ls x)
  (append ls (list x)))
