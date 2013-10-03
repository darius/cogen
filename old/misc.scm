(define (assert true?)
  (if (not true?)
      (error "Assertion failed")))

(define (print obj)
  (write obj)
  (newline))

(define (snarf file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((exps '()))
	(let ((exp (read port)))
	  (if (eof-object? exp)
	      (reverse exps)
	      (loop (cons exp exps))))))))

(define (acons x y l) 
  (cons (cons x y) l))

(define (aupdate key value a-list)
  (acons key value
	 (filter (lambda (pair) (not (eq? (car pair) key)))
		 a-list)))

(define (filter test ls)
  (cond ((null? ls) '())
	((test (car ls))
	 (cons (car ls) (filter test (cdr ls))))
	(else
	 (filter test (cdr ls)))))

(define (all-true? ls)
  (or (null? ls)
      (and (car ls) (all-true? (cdr ls)))))

(define (flatten ls)
  (if (null? ls)
      '()
      (append (car ls)
	      (flatten (cdr ls)))))

(define (foldl fn id ls)
  (if (null? ls)
      id
      (foldl fn (fn id (car ls)) (cdr ls))))
