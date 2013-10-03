(define (snoc ls x)
  (append ls (list x)))

(define (search p _d)
  (_call match p p '() '() _d '()))

; Search p0 against pre0+d, 
; with (p,pre) as our current positions, respectively.
; Invariant: (or (null? d) (not (memv (car d) neg)))
(define (match p0 p pre pre0 _d neg)
  (if (null? p) 
      #t
      (if (null? pre)
	  (if (null? neg)
	      (_if (_op null? _d)
		   #f
		   (call check p0 p pre0 _d neg))
	      (call check p0 p pre0 _d neg))
	  (if (eqv? (car p) (car pre))
	      (call match p0 (cdr p) (cdr pre) pre0 _d neg)
	      (call match p0 p0 (cdr pre0) (cdr pre0) _d neg)))))

(define (check p0 p pre0 _d neg)
  (if (memv (car p) neg)
      (call restart p0 pre0 _d neg)
      (_if (_op eqv? (_lift (car p)) (_op car _d))
	   (_call match p0 (cdr p) '() (call snoc pre0 (car p)) 
		  (_op cdr _d) '())
	   (call restart p0 pre0 _d (cons (car p) neg)))))

(define (restart p0 pre0 _d neg)
  (if (null? pre0)
      (call search p0 (_op cdr _d))
      (call match p0 p0 (cdr pre0) (cdr pre0) _d neg)))
