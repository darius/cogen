(define (snoc ls x)
  (append ls (list x)))

(define (test1)
  (search '(a b a b) '(x a b c a b a c a b a b)))

(define (test2)
  (search '(a b a b c) '(x a b c a b a b a b c)))

(define (test3)
  (search '(a b a b c) '(x a b c a b a b a b d)))


(define (search p d)
  (match p p '() '() d '()))

; Search p0 against pre0+d, 
; with (p,pre) as our current positions, respectively.
; Invariant: (or (null? d) (not (memv (car d) neg)))
; Invariant: (or (not (null? d)) (null? neg))
(define (match p0 p pre pre0 d neg)
  (if (null? p) 
      #t
      (if (null? pre)
	  (if (and (null? neg) (null? d))
	      #f
	      (if (and (not (memv (car p) neg))
		       (eqv? (car p) (car d)))
		  (match p0 (cdr p) '() (snoc pre0 (car p)) (cdr d) '())
		  (if (null? pre0)
		      (search p0 (cdr d))
		      (match p0 p0 (cdr pre0) (cdr pre0) d (cons (car p) neg)))))
	  (if (eqv? (car p) (car pre))
	      (match p0 (cdr p) (cdr pre) pre0 d neg)
	      (match p0 p0 (cdr pre0) (cdr pre0) d neg)))))
