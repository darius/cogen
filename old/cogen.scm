;; A polyvariant compiler generator.
;;   defs: a list of Scheme0 definitions.
;;   f:    the name of a function among those defs
;;   div:  a division of f's arguments into static and dynamic 
;;         (represented by #t and #f, respectively)
;;   residual-fns: a list of which functions in defs to residualize
(define (cogen defs f div residual-fns)

  (define def-table
    (map (lambda (def)
	   (let ((name (def.name def)))
	     (list name def (namer name (lambda (new-name div)
					  (stage-def def new-name div))))))
	 defs))

  (define (get-def op)
    (cond ((assq op def-table) => cadr)
	  (else #f)))

  (define (get-variant name div)
    ((caddr (assq name def-table)) div))

  ; Stage a definition.
  (define (stage-def def residual-name div)
    (let ((params (def.params def))
	  (body (def.body def)))
      (let ((statics (choose div params))
	    (dynamics (choose (map not div) params))
	    (body-exp (result.exp (stage-exp (map cons params div) body))))
	(emit-code
	 ((if (memq (def.name def) residual-fns) build-folded build-unfolded)
	  residual-name statics dynamics body-exp)))))

  ; Stage expression e in environment r, returning its binding time
  ; and its staged version.
  (define (stage-exp r e)
    (cond ((literal? e) (make-result #t e))
	  ((symbol? e) (if (cdr (assq e r)) 
			   (make-result #t e)
			   (make-result #f `',e)))
	  (else (let* ((results (map (lambda (e) (stage-exp r e)) (cdr e)))
		       (div (map result.static? results))
		       (args (map result.exp results))
		       (call? (get-def (car e)))
		       (op (if call? (get-variant (car e) div) (car e))))
		  (if (all-true? div)
		      (make-result #t `(,op ,@args))
		      (make-result #f (dynamize call? op div args)))))))

  ; Return code to emit a compound expression.
  (define (dynamize call? op div args)
    (cond (call? `(make-call (,op ,@(choose div args))
			     ,@(choose (map not div) args)))
	  ((and (eq? op 'if) (car div))
	   `(if ,(car args) ,@(map lift (cdr div) (cdr args))))
	  (else `(list ',op ,@(map lift div args)))))

  (get-variant f div))

(define (build-unfolded name statics dynamics body)
  `(define (,name ,@statics)
     ,(if (null? dynamics)
	  body
	  `(list 'lambda ',dynamics ,body))))

(define (build-folded name statics dynamics body)
  (let ((f (fresh-variable "f")))
    `(define ,name
       (namer ',name
	      (lambda (,f ,@statics)
		(emit-code (list 'define (cons ,f ',dynamics)
				 ,body)))))))
