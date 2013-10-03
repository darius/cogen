(define (make-call f . operands)
  (if (symbol? f)
      `(,f ,@operands)
      (make-let (cadr f) operands (caddr f))))

(define (make-let vars vals body)
  (let ((bindings (flatten (map (lambda (var val)
				  (if (eq? var val) '() `((,var ,val))))
				vars vals))))
    (if (null? bindings)
	body
	`(let ,bindings ,body))))

(define make-code      cons)
(define bind-time      car)
(define lisp           cdr)

(define (static? code) (eq? (bind-time code) 'S))

(define (lift code)
  (if (static? code)
      `(make-literal ,(lisp code)) 
      (lisp code)))

(define (lub x y)
  (case x
    ((D) 'D)
    ((L) (if (eq? y 'D) 'D 'L))
    ((S) y)))

(define (lub* xs)
  (foldl lub 'S xs))

(define (choose x xs values)
  (cond ((null? values) '())
	((eq? x (car xs))
	 (cons (car values)
	       (choose x (cdr xs) (cdr values))))
	(else (choose x (cdr xs) (cdr values)))))

(define (all-static? es)
  (all-true? (map static? es)))

(define (congruent? expected supplied)
  (all-true? (map (lambda (x y) (eq? (lub x y) x))
		  expected
		  supplied)))

(define (namer name observe-update)
  (let ((table '()))
    (lambda args
      (cond ((assoc args table) => cdr)
            (else (let ((new-name (fresh-variable name)))
                    (set! table (acons args new-name table))
                    (apply observe-update new-name args)
                    new-name))))))

(define (emit-code form)
  (print form)
  (eval-top form))
