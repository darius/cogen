;; A trivial 'compiler generator'.
;;
;; Just copy the input program. But do this in stages: first return a
;; Scheme expression that when evaluated and called will return the
;; copy. This would still be completely trivial, except we walk the
;; input and build the output in a way suited to the bigger changes
;; that we'll want later elaborations of this code to make. For now
;; the only such change is renaming the functions from the input
;; program; this is needed because a function from the input turns
;; into two functions, one for each output stage (the 'generating
;; extension' and the 'residual program'); plus in later elaborations
;; we'll generate more versions, specializing the function to
;; particular inputs.

(load "syntax.scm")

;; I may be missing the right way to look at it, but I can't seem to
;; find any working expression of all the logic below as nested
;; quasiquotes using Guile's built-in quasiquote, which seems to be a
;; standard one for Scheme. So here I give up and substitute Norvig's
;; quasiquote, ported to Scheme.
(load "quasiquote.scm")

(define (stage defs main-name)
  (let ((dn (map-def.name defs))
        (dv defs))

    (define (stage-def def)
      (let ((name   (def.name def))
            (params (def.params def))
            (body   (def.body def)))
        `(define (,(make-name name 1))
           `(define (,',(make-name name 2) ,@',params)
              ,,(stage-exp body)))))

    (define (stage-exp e)
      (cond ((symbol? e) `',e)
            ((not (pair? e)) e)
            ((eq? (car e) 'quote) ``',,e)
            (else (let ((operator (car e))
                        (operands (map stage-exp (cdr e))))
                    (let ((rator (if (memq operator dn)
                                     (make-name operator 2)
                                     operator)))
                      ``(,',rator ,,@operands))))))

    `(lambda (result-name)
       ,@(map stage-def defs)
       ,(let ((params (def.params (lookup-def main-name dn dv))))
          ``((define (,result-name ,@',params)
               (,',(make-name main-name 2) ,@',params))
             ,,@(map (lambda (name) `(,(make-name name 1)))
                     dn))))))

(define (make-name name stage)
  (string->symbol (string-append (symbol->string name)
                                 "_"
                                 (number->string stage))))

;; Smoke tests

(load "utils.scm")

(define pows (snarf "programs/power.scm"))
(define gen-power-code (stage pows 'power))
;(pretty-print gen-power-code)
(define gen-power (evil gen-power-code))
(define power-code (gen-power 'greyskull))
(define expected-power
  '((define (greyskull x n) (power_2 x n))
    (define (power0_2 x n)
      (if (= n 0) 1 (* x (power0_2 x (- n 1)))))
    (define (power_2 x n)
      (if (= n 0)
          1
          (if (odd? n)
              (* x (power_2 x (- n 1)))
              (square_2 (power_2 x (/ n 2))))))
    (define (square_2 y) (* y y))))
(cond ((not (equal? power-code expected-power))
       (display "Failed") (newline)
       (pretty-print power-code)))

(define appends (snarf "programs/append.scm"))
(define gen-append-code (stage appends 'append))
;(pretty-print gen-append-code)
(define append-code ((evil gen-append-code) 'penned))
(define expected-append
  '((define (penned prefix suffix) (append_2 prefix suffix))
    (define (append_2 prefix suffix)
      (if (null? prefix)
          suffix
          (cons (car prefix)
                (append_2 (cdr prefix) suffix))))))
(cond ((not (equal? append-code expected-append))
       (display "Failed") (newline)
       (pretty-print append-code)))
