;; The plan:
;;
;; (stage input-program function-name signatures)
;;   -> generating-extension, a Scheme lambda-expression of type: 
;;      arguments -> residual-program
;; where RESIDUAL-PROGRAM has just the definitions from SIGNATURES.
;; Each signature is a function name and parameter list from
;; PROGRAM, with some parameters possibly omitted. 
;; ARGUMENTS includes one value for each parameter omitted 
;; from the signature for FUNCTION-NAME.
;;
;; The contract: GENERATING-EXTENSION takes arguments for some of the
;; parameters to FUNCTION-NAME and produces a residual program taking
;; the remaining parameters, and behaving like INPUT-PROGRAM on all of
;; the parameters together.
;;
;; There must be a signature for FUNCTION-NAME so we know which
;; parameters to assign to the generating extension and which to the
;; residual. For the other signatures, it's up to the stager whether
;; to take them as a hint or a hard requirement; if the latter, and it
;; can't find a way to satisfy the requirement, STAGE may fail and
;; complain. Here, TRIVIALLY-STAGE never complains.
;;
;; Note that INPUT-PROGRAM and RESIDUAL-PROGRAM are to be in
;; first-order Scheme, but GENERATING-EXTENSION in between is in
;; unrestricted Scheme -- in practice the main addition will be the
;; LET form, so we could make it all uniform with a little extra work,
;; I expect. (There'd be no real point in coding STAGE itself in
;; first-order Scheme, though.)

(define (trivially-stage input-program function-name signatures)
  (let ((defs input-program)
        (names (map-def.name input-program))
        (residual-params (cdr (assq function-name signatures))))
    (let ((def (lookup-def function-name names defs)))
      (let ((generating-params (difference (def.params def) residual-params)))
        `(lambda ,generating-params
           (cons ,(trivially-stage-def function-name residual-params (def.body def))
                 ',(difference defs (list def))))))))

(define (trivially-stage-def name residual-params body)
  ;; (Is Guile buggy at nested quasiquotes, or is my understanding wrong?)
  `(list 'define 
         ',(cons name residual-params)
         ,(trivially-stage-exp residual-params body)))

(define (trivially-stage-exp residual-params e)
  (let staging ((e e))
    (cond ((literal? e) `',e)
          ((symbol? e)
           (if (memq e residual-params)
               `',e
               `(list 'quote ,e)))
          (else (let ((operator (car e))
                      (operands (map staging (cdr e))))
                  `(list ',operator ,@operands))))))

(define (difference things unwanted)
  ;; THINGS without the UNWANTED, preserving the order of THINGS.
  (filter (lambda (thing) (not (member thing unwanted)))
          things))

(define (filter ok? xs)
  (foldr (lambda (x result)
           (if (ok? x) (cons x result) result))
         '()
         xs))

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs)
         (foldr f z (cdr xs)))))
