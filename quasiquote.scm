;; Expand quasiquote forms. Based on quasi-q from
;; http://norvig.com/paip/compile3.lisp

;; This handles nested quasiquotes like Alan Bawden 1999,
;; "Quasiquotation in Lisp", and unlike the Scheme standard and actual
;; Scheme systems I've tried. This behavior here seems better:
;; there are simple things I couldn't figure out at all for the
;; built-in quasiquote.

;; N.B. the expansion is not hygienic: it references QUOTE, QQ:LIST,
;; etc., without regard to scope. TODO when really bored: fix this

;; A Guile macro. Dunno what other Schemes you might luck out with.
(defmacro quasiquote (x)
  (qq-expand x))

(define (qq-expand x)
  (cond ((vector? x)
	 (cons 'list->vector (qq-expand (vector->list exp))))
        ((or (null? x) (symbol? x))
         (list 'quote x))
        ((not (pair? x))
         x)
        ((starts-with? x 'unquote)
         (cadr x))
        ((starts-with? x 'unquote-splicing)
         (error "Bad syntax in quasiquote" x))
        ((starts-with? x 'quasiquote)
         (qq-expand (qq-expand (cadr x))))
        ((starts-with? (car x) 'unquote-splicing)
         (qq-append (cadr (car x))
                    (qq-expand (cdr x))))
        (else (qq-cons (qq-expand (car x))
                       (qq-expand (cdr x))
                       x))))

(define (qq-append spliceme expansion)
  (if (equal? expansion ''())
      spliceme
      (list 'qq:append spliceme expansion)))

(define (qq-cons left right x)
  (cond ((and (qq-constant? left) (qq-constant? right))
         (list 'quote (reuse-cons (qq-eval left) (qq-eval right) x)))
        ((equal? right ''())
         (list 'qq:list left))
        ((starts-with? right 'qq:list)
         (cons 'qq:list (cons left (cdr right))))
        (else
         (list 'qq:cons left right))))

(define qq:list list)
(define qq:cons cons)
(define qq:append append)

(define (reuse-cons new-car new-cdr pair)
  (if (and (eqv? new-car (car pair))
           (eqv? new-cdr (cdr pair)))
      pair
      (cons new-car new-cdr)))

(define (qq-constant? exp)
  (if (pair? exp)
      (eq? (car exp) 'quote)
      (not (symbol? exp))))

(define (qq-eval constant)
  (if (pair? constant)       ;; must be quoted constant
      (cadr constant)
      constant))

(define (starts-with? x tag)
  (and (pair? x) (eq? (car x) tag)))
