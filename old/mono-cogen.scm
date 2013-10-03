;; A monovariant compiler generator.
;; I forget how complete this is...
;; And I'm not sure yet why this seems to have more code than the polyvariant
;; cogen.scm.

(define (make-program defs divs)	;kind of a lousy name...
  (map (lambda (def div)
	 (let ((name (def.name def)))
	   (list name def div)))
       defs divs))

(define (get-def op program)
  (cond ((assq op program) => cadr)
	(else #f)))

(define (get-div name program)
  (caddr (assq name program)))

(define (stage-em defs program)
  (map (lambda (d) (stage d program)) defs))

(define (stage def program)
  (let ((name (def.name def))
	(params (def.params def))
	(body (def.body def)))
    (let ((div (get-div name program)))
      (let ((statics (choose 'S div params))
	    (dynamics (choose 'D div params))
	    (r (map cons params div)))
	`(define (,name ,@params)
	   ,(lisp (stage-exp body #t r program)))))))

(define (stage-exp e unf? r program)
  (cond ((literal? e) (make-code 'S e))
	((symbol? e) (make-code (cdr (assq e r)) e))
	((eq? (car e) 'if)
	 (stage-if (cadr e) (caddr e) (cadddr e) unf? r program))
	(else (stage-app (car e) (cdr e) unf? r program))))

(define (stage-if test then els unf? r program)
  (let* ((test (stage-exp test unf? r program))
	 (static-test? (static? test))
	 (then (stage-exp then (and unf? static-test?) r program))
	 (els  (stage-exp els  (and unf? static-test?) r program)))
    (cond ((all-static? (list test then els))
	   (make-code 'S `(if ,(lisp test) ,(lisp then) ,(lisp els))))
	  (static-test?
	   (make-code 'D `(if ,(lisp test) ,(lift then) ,(lift els))))
	  (else
	   (make-code 'D `(list 'if ,(lift test) ,(lift then) ,(lift els)))))))

(define (stage-app operator operands unf? r program)
  (let ((args (map (lambda (e) (stage-exp e unf? r program))
		   operands)))
    (cond ((get-def operator program)
	   (stage-call unf? operator args program))
	  ((all-static? args)
	   (make-code 'S `(,operator ,@(map lisp args))))
	  (else
	   (make-code 'D `(list ',operator ,@(map lift args)))))))

(define (stage-call unf? op args program)
  (let ((expected (get-div op program)))
    (assert (congruent? expected (map bind-time args)))
    (let ((static-args (map lisp (choose 'S expected args)))
	  (dynamic-args (map lift (choose 'D expected args))))
      (if (eq? 'S (lub* expected))
	  (make-code 'S `(,op ,@static-args))
	  (make-code 'D (dynamic-call unf? op expected args program))))))

(define (dynamic-call unfold? op expected args program)
  (let* ((params (def.params (get-def op program)))
	 (lisp-args (map (lambda (e a p) (if (eq? 'S e) (lisp a) `',p))
			 expected
			 args
			 params))
	 (dynamic-args (map lift (choose 'D expected args))))
    (if unfold?
	`(make-let ',(choose 'D expected params) 
		   (list ,@dynamic-args)
		   (,op ,@lisp-args))
	`(list (fold-call ',op ',params ',expected ,op (list ,@lisp-args))
	       ,@dynamic-args))))

(define (fold-call name params div proc args)
  (let ((key (cons name (choose 'S div args))))
    (cond ((assoc key memo-table) => cdr)
	  (else (let ((specialized-name (fresh-variable name)))
		  (set! memo-table
			(acons key specialized-name memo-table))
		  (emit-code `(define (,specialized-name 
					,@(choose 'D div params))
				,(apply proc args)))
		  specialized-name)))))

(define memo-table '())

(define (specialize name args program)
  (fold-call name
	     (def.params (get-def name program))
	     (get-div name program)
	     (eval-top name)
	     args))
