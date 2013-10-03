(define (stage def)
  (let ((name (def.name def))
	(params (def.params def))
	(body (def.body def)))
    `(define (,name)
       `(define ,',name 
	  (lambda ,',params
	    ,,(lift-result (stage-exp body)))))))

(define (stage-exp e)
  (cond ((literal? e) (make-result #t e))
	((symbol? e) (make-result #f `',e))
	(else (let ((operator (car e))
		    (results (map stage-exp (cdr e))))
		(let ((div (map result.static? results))
		      (args (map result.exp results)))
		  (if (and (all-true? div) (pure? operator))
		      (make-result #t `(,operator ,@args))
		      (make-result #f (dynamize operator div args))))))))

(define (dynamize op div args)
  (cond ((and (eq? op 'if) (car div))
	 `(if ,(car args) ,@(map lift (cdr div) (cdr args))))
	(else
	 `(list ',op ,@(map lift div args)))))

; TODO: try this over with a different representation
; (constants as constant expressions, dynamics as general expressions)
; Also, it'd reflect future development better if we passed along a
; defs table to distinguish calls from primitives.

(define make-result    cons)
(define result.static? car)
(define result.exp     cdr)

(define (lift-result result)
  (lift (result.static? result) (result.exp result)))

(define (lift static? exp)
  (if static? `(make-literal ,exp) exp))

(define (pure? f)
  (or (eq? f 'if)
      (memq f primitive-names)))
