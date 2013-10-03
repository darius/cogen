(define fresh-variable-counter -1)

(define (fresh-variable stem)
  (set! fresh-variable-counter (+ fresh-variable-counter 1))
  (string->symbol
   (string-append (if (string? stem) stem (symbol->string stem))
		  "_"
		  (number->string fresh-variable-counter))))

(define (def? x) (and (pair? x) (eq? (car x) 'define)))
(define def.name caadr)
(define def.params cdadr)
(define def.body caddr)

(define (map-def.name defs)
  (if (null? defs)
      '()
      (cons (def.name (car defs))
	    (call map-def.name (cdr defs)))))

(define (literal? x)
  (if (pair? x)
      (eq? (car x) 'quote)
      (not (symbol? x))))

(define (literal-value x)
  (if (pair? x)
      (cadr x)
      x))

(define (make-literal value)
  (if (or (pair? value) (null? value) (symbol? value))
      (list 'quote value)
      value))
