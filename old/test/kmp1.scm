(define (snoc ls x)
  (append ls (list x)))

(define (search p d)
  (call match p p '() '() d '()))

; Search p0 against pre0+d, 
; with (p,pre) as our current positions, respectively.
; Invariant: (or (null? d) (not (memv (car d) neg)))
(define (match p0 p pre pre0 d neg)
  (if (null? p) 
      #t
      (if (null? pre)
	  (if (null? d)
	      #f
	      (if (memv (car p) neg)
		  (call restart p0 pre0 d neg)
		  (if (eqv? (car p) (car d))
		      (call match p0 (cdr p) '() (call snoc pre0 (car p)) (cdr d) '())
		      (call restart p0 pre0 d (cons (car p) neg)))))
	  (if (eqv? (car p) (car pre))
	      (call match p0 (cdr p) (cdr pre) pre0 d neg)
	      (call match p0 p0 (cdr pre0) (cdr pre0) d neg)))))

(define (restart p0 pre0 d neg)
  (if (null? pre0)
      (call search p0 (cdr d))
      (call match p0 p0 (cdr pre0) (cdr pre0) d neg)))
