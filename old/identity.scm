;; A perfectly trivial 'compiler generator'.
;;
;; None of the definition's parameters are static, hence all are
;; dynamic; we return code for a function that takes no static 
;; parameters and returns a copy of the definition with all
;; dynamic parameters.
;;
;; In fancier elaborations we'll have more smarts in the copying
;; part, etc.

(define (stage def)
  (let ((name (def.name def))
	(params (def.params def))
	(body (def.body def)))
    `(define (,name)
       `(define ,',name 
	  (lambda ,',params
	    ,,(stage-exp body))))))

(define (stage-exp e)
  (cond ((literal? e) `(make-literal ,e))
	((symbol? e) `',e)
	(else (let ((operator (car e))
		    (operands (map stage-exp (cdr e))))
		`(list ',operator ,@operands)))))
