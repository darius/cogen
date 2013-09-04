;; Like 1-echo.scm, but now the result will only include definitions
;; reachable from the main one.

(load "syntax.scm")
(load "quasiquote.scm")

(define (stage defs main-name)
  (let ((dn (map-def.name defs))
        (dv defs)
        (live '()))

    (define (stage-function! name)
      (cond ((assq name live))
            (else (let ((pair (cons name '*)))
                    (set! live (cons pair live))
                    (set-cdr! pair (stage-def (lookup-def name dn dv)))))))

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
                    (let ((rator (cond ((memq operator dn)
                                        (stage-function! operator)
                                        (make-name operator 2))
                                       (else operator))))
                      `(list ',rator ,@operands))))))

    (stage-function! main-name)
    `(lambda (result-name)
       ,@(map cdr live)
       ,(let ((params (def.params (lookup-def main-name dn dv))))
          ``((define (,result-name ,@',params)
               (,',(make-name main-name 2) ,@',params))
             ,,@(map (lambda (pair) `(,(make-name (car pair) 1)))
                     live))))))

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
    (define (square_2 y) (* y y))
    (define (power_2 x n)
      (if (= n 0)
          1
          (if (odd? n)
              (* x (power_2 x (- n 1)))
              (square_2 (power_2 x (/ n 2))))))))
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
