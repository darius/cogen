;; terp.scm doesn't just load this file because terp.scm is also run
;; metacircularly, and it doesn't have a LOAD primitive.

(define (def.name def)   (caadr def))
(define (def.params def) (cdadr def))
(define (def.body def)   (caddr def))

(define (map-def.name defs)
  (if (null? defs)
      '()
      (cons (def.name (car defs))
            (map-def.name (cdr defs)))))

(define (lookup-def name dn dv)
  (if (null? dn)
      (error "Unknown function" name)
      (if (eq? name (car dn))
          (car dv)
          (lookup-def name (cdr dn) (cdr dv)))))
