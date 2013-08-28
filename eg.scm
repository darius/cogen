(define (make-gen receiver)
  (let ((defs '()))
    (define (make-namer stem residual-params gen-body)
      (let ((memo '())
            (stem (make-name (cons stem residual-params))))
        (lambda arguments
          (cond ((assoc arguments memo) => cadr)
                (else (let ((name (make-name (list stem (length memo)))))
                        (set! memo (cons (list arguments name) memo))
                        (set! defs (cons `(define (,name ,@residual-params)
                                            ,(apply gen-body arguments))
                                         defs))
                        name))))))
    (define (list-defs entry-point)
      (list entry-point defs))
    (receiver make-namer list-defs)))

(define (make-name parts)
  (string->symbol
   (apply string-append
          (foldr (lambda (part strings)
                   (cons (cond ((symbol? part) (symbol->string part))
                               ((number? part) (number->string part))
                               (else part))
                         (if (null? strings)
                             '()
                             (cons "_" strings))))
                 '()
                 parts))))

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs)
         (foldr f z (cdr xs)))))

;; What staging should produce for programs/power.scm
(define gen-power
  (make-gen
   (lambda (make-namer list-defs)
     (define power_n
       (make-namer 'power '(x)
                   (lambda (n)
                     (if (= n 0)
                         1
                         (if (odd? n)
                             `(* x (,(power_n (- n 1)) x))
                             `(,(square) (,(power_n (/ n 2)) x)))))))
     (define square
       (make-namer 'square '(y)
                   (lambda ()
                     `(* y y))))
     (lambda (n)
       (let ((entry-point (power_n n)))
         (list-defs entry-point))))))
